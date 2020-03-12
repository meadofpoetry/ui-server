include Ptime

module I64 = struct
  include Int64
  let ( * ) = mul
  let ( / ) = div
end

let to_human_string ?tz_offset_s (t : t) =
  let (y, m, d), ((h, min, s), _) = to_date_time ?tz_offset_s t in
  Printf.sprintf "%02d.%02d.%04d %02d:%02d:%02d" d m y h min s

let of_human_string_exn ?(tz_offset_s = 0) s =
  match String.split_on_char ' ' s with
  | [date; time] ->
     let y, m, d = match String.split_on_char '.' date with
       | [day; month; year] ->
          int_of_string year,
          int_of_string month,
          int_of_string day
       | _ -> failwith "bad date value(s)" in
     let h, min, s = match String.split_on_char ':' time with
       | [hour; min; sec] ->
          int_of_string hour,
          int_of_string min,
          int_of_string sec
       | _ -> failwith "bad time value(s)" in
     of_date_time ((y, m, d), ((h, min, s), tz_offset_s)) |> Option.get
  | _ -> failwith "not a human-readable date time string"

(* TODO
let to_yojson' (v : t) : Yojson.Safe.t =
  let d, ps = Ptime.to_span v |> Ptime.Span.to_d_ps in
  `List [`Int d; `Intlit (Int64.to_string ps)]

let of_yojson' (j : Yojson.Safe.t) : (t, string) result =
  let to_err j =
    Printf.sprintf "of_yojson: bad json value (%s)"
    @@ Yojson.Safe.to_string j in
  match j with
  | `List [d; ps] ->
     Result.(
      Json.Int.of_yojson d
      >>= fun d -> Json.Int64.of_yojson ps
      >>= fun ps -> return (v (d,ps)))
  | _ -> Error (to_err j)
 *)
let to_yojson (v : t) : Yojson.Safe.t =
  let t = to_rfc3339 ~frac_s:6 v in
  `String t

let of_yojson (j : Yojson.Safe.t) : (t, string) result =
  let to_err j =
    Printf.sprintf "of_yojson: bad json value (%s)"
    @@ Yojson.Safe.to_string j in
  match j with
  | `String s ->
     begin match of_rfc3339 s with
     | Ok (t, _, _) -> Ok t
     | Error _ -> Error "of_yojson: bad rfc3339 string"
     end
  | _ -> Error (to_err j)

module Interval = Interval

let split ~from ~till =
  let second = 1 in
  let minute = second * 60 in
  let hour = minute * 60 in
  let day  = hour * 24 in
  let week = day * 7 in
  let year = day * 365 in
  let check_diff dif =
    if Span.compare dif (Span.of_int_s (year * 3)) > 0 then `Years
    else if Span.compare dif (Span.of_int_s (week * 3)) > 0 then `Weeks
    else if Span.compare dif (Span.of_int_s (day * 3)) > 0 then `Days
    else if Span.compare dif (Span.of_int_s (hour * 2)) > 0 then `Hours
    else if Span.compare dif (Span.of_int_s minute) > 0 then `Minutes
    else `Seconds
  in
  let sep_days ~sz (fd,fps) (td,tps) =
    if sz <= 0 then failwith "invariant is broken";
    let rec loop acc d =
      if d > (td - sz) then ((d,0L),(td,tps))::acc
      else let nd = d + sz in loop (((d,0L),(nd,0L))::acc) nd
    in let (d,dps) = fd + 1, 0L in
       ((fd,fps),(d,dps)) :: (List.rev @@ loop [] d)
  in
  let sep_seconds ~sz from til =
    if sz <= 0 then failwith "invariant is broken";
    let sz = Span.of_int_s sz in
    let rec loop acc st =
      if compare st (sub_span til sz |> Option.get) > 0 then (st,til)::acc
      else let nst = Option.get @@ add_span st sz in
           loop ((st,nst)::acc) nst
    in let s = Option.get @@ add_span (truncate ~frac_s:0 from) (Span.of_int_s 1) in
       (from,s)::(List.rev @@ loop [] s)
  in
  let merge (from, til) = unsafe_of_d_ps from, unsafe_of_d_ps til in
  let dif = diff till from in
  let fd,fps = Span.to_d_ps @@ to_span from in
  let td,tps = Span.to_d_ps @@ to_span till in
  match check_diff dif with
  | `Years   -> List.map merge @@ sep_days ~sz:365 (fd,fps) (td,tps)
  | `Weeks   -> List.map merge @@ sep_days ~sz:7 (fd,fps) (td,tps)
  | `Days    -> List.map merge @@ sep_days ~sz:1 (fd,fps) (td,tps)
  | `Hours   -> sep_seconds ~sz:3600 from till
  | `Minutes -> sep_seconds ~sz:60 from till
  | `Seconds -> [from, till]

let ps_in_s = 1000_000_000_000L

module Conv (M : sig
    val of_int : int -> int * int64
    val to_int : int * int64 -> int
  end) = struct
  type t = Ptime.t

  let of_int x =
    Option.get
    @@ Ptime.of_span
    @@ Option.get
    @@ Ptime.Span.of_d_ps (M.of_int x)

  let to_int x =
    M.to_int
    @@ Ptime.Span.to_d_ps
    @@ Ptime.to_span x

  let of_string s = of_int @@ int_of_string s
  let to_string x = string_of_int @@ to_int x

  let of_yojson x =
    match x with
    | `Int x -> Ok (of_int x)
    | _ -> Error "of_yojson"
    | exception _ -> Error "of_yojson"
  let to_yojson x = `Int (to_int x)
end

module Conv64 (M : sig val second : int64 end) = struct
  let () =
    if Int64.compare M.second ps_in_s > 0
    then failwith "Time.Span.Conv64: second precision is more than 1ps"

  type t = Ptime.t

  let of_int64 (x : int64) : t =
    let s = (Int64.to_float x) /. (Int64.to_float M.second) in
    try
      Option.get
      @@ Ptime.of_span
      @@ Option.get
      @@ Ptime.Span.of_float_s s
    with e ->
          Printf.printf "Can't convert a num %Ld\n" x;
          raise e

  let to_int64 (x : t) : int64 =
    Ptime.Span.to_float_s @@ Ptime.to_span x
    |> ( *. ) (Int64.to_float M.second)
    |> Int64.of_float

  let of_string (s : string) : t =
    of_int64 @@ Int64.of_string s

  let to_string (x : t) : string =
    Int64.to_string @@ to_int64 x

  let of_yojson (x : Yojson.Safe.t) : (t, string) result = match x with
    | `Intlit x -> Ok (of_string x)
    | `Int x -> Ok (of_int64 @@ Int64.of_int x)
    | _ -> Error "of_yojson"
    | exception _ -> Error "of_yojson"

  let to_yojson (x : t) : Yojson.Safe.t =
    `Intlit (to_string x)

end

module Hours = Conv(struct
    let of_int x = (x / 24, I64.(of_int (x mod 24) * 3600L * ps_in_s))
    let to_int (d,ps) = (d * 24) + I64.(to_int (ps / (3600L * ps_in_s)))
  end)

module Seconds = Conv(struct
    let of_int x = Ptime.Span.to_d_ps @@ Ptime.Span.of_int_s x
    let to_int x = Option.get @@ Ptime.Span.to_int_s @@ Ptime.Span.v x
  end)

module Seconds64 = Conv64(struct let second = 1L end)

module Useconds = Conv64(struct let second = 1000_000L end)

module Period = struct
  include Ptime.Span

  let to_yojson (v:t) : Yojson.Safe.t =
    let d, ps = Ptime.Span.to_d_ps v in
    `List [ `Int d;`Intlit (Int64.to_string ps) ]

  let of_yojson (j:Yojson.Safe.t) : (t,string) result =
    let to_err j = Printf.sprintf "span_of_yojson: bad json value (%s)" @@ Yojson.Safe.to_string j in
    match j with
    | `List [ `Int d; `Intlit ps] ->
      (match Int64.of_string_opt ps with
       | Some ps -> Option.to_result ~none:"None" (Ptime.Span.of_d_ps (d,ps))
       | None    -> Error (to_err j))
    | _ -> Error (to_err j)

  module Conv (M : sig
               val of_int : int -> int * int64
               val to_int : int * int64 -> int
             end) = struct
    type t = Ptime.Span.t

    let of_int x = Option.get @@ Ptime.Span.of_d_ps (M.of_int x)
    let to_int x = M.to_int @@ Ptime.Span.to_d_ps x
    let of_string s = of_int @@ int_of_string s
    let to_string x = string_of_int @@ to_int x
    let of_yojson x = match x with `Int x -> Ok(of_int x)
                                 | _ -> Error "of_yojson"
                                 | exception _ -> Error "of_yojson"
    let to_yojson x = `Int (to_int x)
  end

  module Conv64 (M : sig val second : int64 end) = struct
    let () =
      if Int64.compare M.second ps_in_s > 0
      then failwith "Time.Span.Conv64: second precision is more than 1ps"

    type t = Ptime.Span.t

    let of_int64 (x : int64) : t =
      Ptime.Span.of_float_s ((Int64.to_float x) /. (Int64.to_float M.second))
      |> Option.get
    (* let d  = Int64.(to_int (x / (24L * 60L * 60L * M.second))) in
     * let ps = Int64.((x mod (24L * 60L * 60L)) * (ps_in_s / M.second)) in
     * Option.get_exn
     * @@ Option.flat_map Ptime.of_span (Ptime.Span.of_d_ps (d, ps)) *)

    let to_int64 (x : t) : int64 =
      Ptime.Span.to_float_s x
      |> ( *. ) (Int64.to_float M.second)
      |> Int64.of_float
    (* let d, ps = Ptime.Span.to_d_ps @@ Ptime.to_span x in
     * let d = Int64.((of_int d) * (24L * 60L * 60L * M.second)) in
     * let ps = Int64.((ps * M.second) / ps_in_s) in
     * Int64.(d + ps) *)

    let of_string (s : string) : t =
      of_int64 @@ Int64.of_string s
    let to_string (x : t) : string =
      Int64.to_string @@ to_int64 x

    let of_yojson (x : Yojson.Safe.t) : (t, string) result = match x with
      | `Intlit x -> Ok(of_string x)
      | `Int x -> Ok(of_int64 @@ Int64.of_int x)
      | _ -> Error "of_yojson"
      | exception _ -> Error "of_yojson"

    let to_yojson (x : t) : Yojson.Safe.t =
      `Intlit (to_string x)

  end

  module Hours =
    Conv(struct
      let of_int x = (x / 24, I64.(of_int (x mod 24) * 3600L * ps_in_s))
      let to_int (d,ps) = (d * 24) + I64.(to_int (ps / (3600L * ps_in_s)))
    end)

  module Seconds =
    Conv(struct
      let of_int x = Ptime.Span.to_d_ps @@ Ptime.Span.of_int_s x
      let to_int x = Option.get @@ Ptime.Span.to_int_s @@ Ptime.Span.v x
    end)

  module Seconds64 = Conv64(struct let second = 1L end)

  module Useconds = Conv64(struct let second = 1000_000L end)

end

module Range = struct
  type t = Ptime.t * Ptime.Span.t

  let after time span : t = (time, span)

  let equal (a : t) (b : t) =
    equal (fst a) (fst b) && Period.equal (snd a) (snd b)

  let to_yojson (t, s : t) : Yojson.Safe.t =
    `List [to_yojson t; Period.to_yojson s]

  let of_yojson : Yojson.Safe.t -> (t, string) result = function
    | `List [t; s] ->
      (match of_yojson t, Period.of_yojson s with
       | Ok t, Ok s -> Ok (t, s)
       | _ -> Error "of_yojson")
    | _ -> Error "of_yojson"

end

module Relative = struct
  include Ptime.Span

  let to_seconds : t -> int = fun x -> Option.get @@ Ptime.Span.to_int_s x

  let of_seconds : int -> t = Ptime.Span.of_int_s

end

