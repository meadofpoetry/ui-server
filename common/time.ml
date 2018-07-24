open Containers

include Ptime

module Clock = struct

  let now () = match of_float_s @@ Unix.gettimeofday () with
    | Some x -> x
    | None   -> assert false

  let now_s () = match of_float_s @@ Unix.time () with
    | Some x -> x
    | None   -> assert false

end

let to_human_string ?tz_offset_s (t:t) =
  let (y,m,d),((h,min,s),_) = to_date_time ?tz_offset_s t in
  Printf.sprintf "%02d.%02d.%04d %02d:%02d:%02d" d m y h min s

let of_human_string_exn ?(tz_offset_s=0) s =
  match String.split_on_char ' ' s with
  | [ date; time ] ->
     let y, m, d = match String.split_on_char '.' date with
       | [ day; month; year ] ->
          int_of_string year,
          int_of_string month,
          int_of_string day
       | _ -> failwith "bad date value(s)" in
     let h, min, s = match String.split_on_char ':' time with
       | [ hour; min; sec ] ->
          int_of_string hour,
          int_of_string min,
          int_of_string sec
       | _ -> failwith "bad time value(s)" in
     of_date_time ((y, m, d), ((h, min, s), tz_offset_s)) |> Option.get_exn
  | _ -> failwith "not a human-readable date time string"

let to_yojson (v:t) : Yojson.Safe.json =
  let d,ps = Ptime.to_span v |> Ptime.Span.to_d_ps in
  `List [ `Int d;`Intlit (Int64.to_string ps) ]

let of_yojson (j:Yojson.Safe.json) : (t,string) result =
  let to_err j = Printf.sprintf "of_yojson: bad json value (%s)" @@ Yojson.Safe.to_string j in
  match j with
  | `List [ d; ps] ->
     Result.(Json.Int.of_yojson d >>= fun d ->
             Json.Int64.of_yojson ps >>= fun ps ->
             return (v (d,ps)))
  | _ -> Error (to_err j)


module Show_RFC3339 = struct
  type t = Ptime.t

  let typ = "RFC3339 timestamp"

  let of_string s =
    of_rfc3339 s |> function Ok (v,_,_) -> v | Error _ -> failwith (Printf.sprintf "RFC3339.of_string: bad input %s" s)

  let to_string s = to_rfc3339 s

  let of_yojson = function
    | `String s -> begin match of_rfc3339 s with
                   | Ok(v,_,_) -> Ok v
                   | Error _   -> Error (Printf.sprintf "RFC3339.of_yojson: bad input %s" s)
                   end
    | _ -> Error (Printf.sprintf "RFC3339.of_yojson: bad input, expected a string")

  let to_yojson v = `String (to_string v)

end

module Show_float = struct
  type t = Ptime.t

  let typ = "UNIX timestamp (float)"

  let of_string x = Option.get_exn @@ Ptime.of_float_s @@ Float.of_string x
  let to_string x = Float.to_string @@ Ptime.to_float_s x

  let of_yojson x = match x with
    | `Float x -> Ok(Option.get_exn @@ Ptime.of_float_s x)
    | `Floatlit x -> Ok(of_string x)
    | _ -> Error (Printf.sprintf "Show_float.of_yojson: bad input, expected a string")
    | exception _ -> Error (Printf.sprintf "Show_float.of_yojson: bad input, expected a string")

  let to_yojson v = `Float (Ptime.to_float_s v)

end

module Show = struct
  module Show_time = Uri_ext.Query.Either(Show_RFC3339)(Show_float)
  type t = Ptime.t
  let typ = Show_time.typ
  let of_string s = match Show_time.of_string s with
    | `Left x -> x | `Right x -> x
  let to_string t = Show_time.to_string (`Left t)
end

let make_interval ?(from:t option) ?(till:t option) ?(duration:span option) () =
  let ok v  = Result.return v in
  let err s = Result.fail s in
  match from,till,duration with
  | Some _,Some _,Some _ -> err "excessive duration query"
  | Some s,Some e,None   -> ok (`Range (s,e))
  | Some s,None,Some d   -> (match add_span s d with
                             | Some e -> ok (`Range (s,e))
                             | None   -> err "time range exceeded")
  | Some s,None,None     -> ok (`Range (s, max))
  | None,Some e,Some d   -> (match sub_span e d with
                             | Some s -> ok (`Range (s,e))
                             | None   -> err "time range exceeded")
  | None,Some e,None     -> ok (`Range (epoch, e))
  | None,None,Some d     -> let e = Clock.now () in
                            (match sub_span e d with
                             | Some s -> ok (`Range (s,e))
                             | None   -> err "time range exceeded")
  | None,None,None       -> ok (`Range (epoch, max))

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
      if compare st (sub_span til sz |> Option.get_exn) > 0 then (st,til)::acc
      else let nst = Option.get_exn @@ add_span st sz in
           loop ((st,nst)::acc) nst
    in let s = Option.get_exn @@ add_span (truncate ~frac_s:0 from) (Span.of_int_s 1) in
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

module Period = struct
  include Ptime.Span

  let ps_in_s = 1000_000_000_000L

  let to_yojson (v:t) : Yojson.Safe.json =
    let d,ps = Span.to_d_ps v in
    `List [ `Int d;`Intlit (Int64.to_string ps) ]

  let of_yojson (j:Yojson.Safe.json) : (t,string) result =
    let to_err j = Printf.sprintf "span_of_yojson: bad json value (%s)" @@ Yojson.Safe.to_string j in
    match j with
    | `List [ `Int d; `Intlit ps] -> (match Int64.of_string_opt ps with
                                      | Some ps -> Result.of_opt (Span.of_d_ps (d,ps))
                                      | None    -> Error (to_err j))
    | _ -> Error (to_err j)
        
  module Conv (M : sig
               val of_int : int -> int * int64
               val to_int : int * int64 -> int
             end) = struct
    type t = Ptime.Span.t
    let typ = "period"
            
    let of_int x = Option.get_exn @@ Ptime.Span.of_d_ps (M.of_int x)
    let to_int x = M.to_int @@ Ptime.Span.to_d_ps x
    let of_string s = of_int @@ int_of_string s
    let to_string x = string_of_int @@ to_int x
    let of_yojson x = match x with `Int x -> Ok(of_int x)
                                 | _ -> Error "of_yojson"
                                 | exception _ -> Error "of_yojson"
    let to_yojson x = `Int (to_int x)
  end

  module Conv64 (M : sig val second : int64 end) = struct
    let () = if Int64.compare M.second ps_in_s > 0
             then failwith "Time.Span.Conv64: second precision is more than 1ps"

    type t = Ptime.Span.t
    let typ = "period"
            
    let of_int64 x =
      let d  = Int64.(to_int (x / (24L * 60L * 60L * M.second))) in
      let ps = Int64.((x mod (24L * 60L * 60L)) * (ps_in_s / M.second)) in
      Option.get_exn @@ Ptime.Span.of_d_ps (d, ps)
    let to_int64 x =
      let d, ps = Ptime.Span.to_d_ps x in
      let d = Int64.((of_int d) * (24L * 60L * 60L * M.second)) in
      let ps = Int64.((ps * M.second) / ps_in_s) in
      Int64.(d + ps)
    let of_string s = of_int64 @@ Int64.of_string_exn s
    let to_string x = Int64.to_string @@ to_int64 x
    let of_yojson x = match x with `Intlit x -> Ok(of_string x)
                                 | `Int x -> Ok(of_int64 @@ Int64.of_int x)
                                 | _ -> Error "of_yojson"
                                 | exception _ -> Error "of_yojson"
    let to_yojson x = `Intlit (to_string x)
  end

  module Hours = Conv(struct
                     let of_int x = (x / 24, Int64.(of_int Int.(x mod 24) * 3600L * ps_in_s))
                     let to_int (d,ps) = (d * 24) + Int64.(to_int (ps / (3600L * ps_in_s)))
                   end)

  module Seconds = Conv(struct
                       let of_int x = Ptime.Span.to_d_ps @@ Ptime.Span.of_int_s x
                       let to_int x = Option.get_exn @@ Ptime.Span.to_int_s @@ Ptime.Span.v x
                     end)

  module Seconds64 = Conv64(struct let second = 1L end)
  module Useconds = Conv64(struct let second = 1000_000L end)
        
end

module Range = struct
  type t = Ptime.t * Ptime.Span.t

  let after time span : t = (time, span)

end

module Relative = struct
  include Ptime.Span

  let typ = "RFC3339 duration"

  let ps_in_s = 1_000_000_000_000L
  let s_in_minute = 60L
  let s_in_hour   = Int64.(s_in_minute * 60L)
  let m_in_hour   = 60L             
             
  let split_units : t -> (int * int * int * int * int) = fun x ->
    let d, ps = Ptime.Span.to_d_ps x in
    let weeks   = d / 7 in
    let days    = d mod 7 in
    let hours   = Int64.(ps / (s_in_hour * ps_in_s)) in
    let minutes = Int64.((ps / (s_in_minute * ps_in_s)) mod m_in_hour) in
    let seconds = Int64.((ps / ps_in_s) mod s_in_minute) in
    weeks, days, Int64.to_int hours, Int64.to_int minutes, Int64.to_int seconds

  let merge_unit weeks days hours minutes seconds : t =
    let days = (weeks * 7) + days in
    let hours = Int64.(of_int hours * s_in_hour * ps_in_s) in
    let minutes = Int64.(of_int minutes * s_in_minute * ps_in_s) in
    let seconds = Int64.(of_int seconds * ps_in_s) in
    Option.get_exn @@ Ptime.Span.of_d_ps (days, Int64.(hours + minutes + seconds))

  let to_seconds : t -> int = fun x -> Option.get_exn @@ Ptime.Span.to_int_s x
  let of_seconds : int -> t = Ptime.Span.of_int_s

  let to_string (x : t) : string =
    let if_z (v, suf) =
      if v = 0 then None else Some (Printf.sprintf "%d%s" v suf)
    in
    if Span.equal Span.zero x
    then "P0S"
    else
      let weeks, days, hours, minutes, seconds = split_units x in
      let big   = List.filter_map if_z [ weeks, "W"; days,  "D"] |> String.concat "" in
      let small = List.filter_map if_z [ hours, "H"; minutes, "M"; seconds, "S"]  |> String.concat "" in
      let part = "P" ^ big in
      if String.is_empty small
      then part
      else part ^ "T" ^ small
              
  let of_string (s:string) : t =
    let open Angstrom in
    let number  = take_while1 (function '0'..'9' -> true | _ -> false)
                  >>= fun x -> return (Some (int_of_string x)) in
    let prefix = string "P" in
    let opt    = option None in
    let sep    = option () (char 'T' *> return ()) in
    let empty  = end_of_input >>= fun () -> return Span.zero in
    let or_O   = function Some x -> x | None -> 0 in
    let value  =
      opt (number <* char 'W') >>= fun weeks ->
      opt (number <* char 'D') >>= fun days ->
      sep *> opt (number <* char 'H') >>= fun hours ->
      opt (number <* char 'M') >>= fun minutes ->
      opt (number <* char 'S') >>= fun seconds ->
      return @@ merge_unit (or_O weeks) (or_O days) (or_O hours) (or_O minutes) (or_O seconds)
    in
    let parser = prefix *> (value <|> empty) in
    s
    |> parse_string parser
    |> function
      | Error e -> failwith e
      | Ok v -> v

end
