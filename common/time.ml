open Containers

module Clock = Ptime_clock
   
include Ptime
      
let to_yojson (v:t) : Yojson.Safe.json =
  let d,ps = Ptime.to_span v |> Ptime.Span.to_d_ps in
  `List [ `Int d;`Intlit (Int64.to_string ps) ]

let of_yojson (j:Yojson.Safe.json) : (t,string) result =
  let to_err j = Printf.sprintf "of_yojson: bad json value (%s)" @@ Yojson.Safe.to_string j in
  match j with
  | `List [ `Int d; `Intlit ps] -> (match Int64.of_string_opt ps with
                                    | Some ps -> Ok (v (d,ps))
                                    | None    -> Error (to_err j))
  | _ -> Error (to_err j)

module Show_RFC3339 = struct
  type t = Ptime.t

  let of_string s =
    of_rfc3339 s |> function Ok (v,_,_) -> v | Error _ -> failwith (Printf.sprintf "RFC3339.of_string: bad input %s" s)

  let to_string = to_rfc3339

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

  let of_string x = Option.get_exn @@ Ptime.of_float_s @@ Float.of_string x
  let to_string x = Float.to_string @@ Ptime.to_float_s x

  let of_yojson x = match x with
    | `Float x -> Ok(Option.get_exn @@ Ptime.of_float_s x)
    | `Floatlit x -> Ok(of_string x)
    | _ -> Error (Printf.sprintf "Show_float.of_yojson: bad input, expected a string")
    | exception _ -> Error (Printf.sprintf "Show_float.of_yojson: bad input, expected a string")

  let to_yojson v = `Float (Ptime.to_float_s v)
                  
end
  
       
module Period = struct
  include Ptime.Span

  let ps_in_s = 1000_000_000_000L
        
  module Conv (M : sig
               val of_int : int -> int * int64
               val to_int : int * int64 -> int
             end) = struct
    type t = Ptime.Span.t
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
