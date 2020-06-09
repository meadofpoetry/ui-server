module I64 = struct
  include Int64

  let ( * ) = mul

  let ( / ) = div

  let ( mod ) = rem
end

module Show_RFC3339 = struct
  type t = Time.t

  let typ = "RFC3339 timestamp"

  let of_string s =
    Time.of_rfc3339 s |> function
    | Ok (v, _, _) -> v
    | Error _ -> failwith (Printf.sprintf "RFC3339.of_string: bad input %s" s)

  let to_string s = Time.to_rfc3339 s

  (*
     let of_yojson = function
       | `String s ->
          begin match Time.of_rfc3339 s with
          | Ok(v, _, _) -> Ok v
          | Error _ -> Error (Printf.sprintf "RFC3339.of_yojson: bad input %s" s)
          end
       | _ -> Error (Printf.sprintf "RFC3339.of_yojson: bad input, expected a string")

     let to_yojson v = `String (to_string v)
  *)
end

module Show_float = struct
  type t = Time.t

  let typ = "UNIX timestamp (float)"

  let of_string x = Option.get @@ Time.of_float_s @@ Float.of_string x

  let to_string (x : t) = Float.to_string @@ Time.to_float_s x

  (*
  let of_yojson x = match x with
    | `Float x -> Ok (get_exn @@ Ptime.of_float_s x)
    | `Floatlit x -> Ok (of_string x)
    | _ -> Error (Printf.sprintf "Show_float.of_yojson: bad input, expected a string")
    | exception _ -> Error (Printf.sprintf "Show_float.of_yojson: bad input, expected a string")

  let to_yojson v = `Float (Ptime.to_float_s v)
 *)
end

module Show = struct
  module Show_time = Netlib.Uri.Query.Either (Show_RFC3339) (Show_float)

  type t = Time.t

  let typ = Show_time.typ

  let of_string s =
    match Show_time.of_string s with `Left x -> x | `Right x -> x

  let to_string t = Show_time.to_string (`Left t)
end

module Show_period = struct
  module Hours = struct
    include Time.Period.Hours

    let typ = "period (hours)"
  end

  module Seconds = struct
    include Time.Period.Seconds64

    let typ = "period (seconds)"
  end

  module Useconds = struct
    include Time.Period.Useconds

    let typ = "period (micro seconds)"
  end
end

module Show_relative = struct
  type t = Time.Relative.t

  let typ = "RFC3339 duration"

  let ps_in_s = 1_000_000_000_000L

  let s_in_minute = 60L

  let s_in_hour = I64.(s_in_minute * 60L)

  let m_in_hour = 60L

  let split_units : t -> int * int * int * int * int =
   fun x ->
    let d, ps = Ptime.Span.to_d_ps x in
    let weeks = d / 7 in
    let days = d mod 7 in
    let hours = I64.(ps / (s_in_hour * ps_in_s)) in
    let minutes = I64.(ps / (s_in_minute * ps_in_s) mod m_in_hour) in
    let seconds = I64.(ps / ps_in_s mod s_in_minute) in
    (weeks, days, Int64.to_int hours, Int64.to_int minutes, Int64.to_int seconds)

  let merge_unit weeks days hours minutes seconds : t =
    let days = (weeks * 7) + days in
    let hours' = I64.(of_int hours * s_in_hour * ps_in_s) in
    let minutes' = I64.(of_int minutes * s_in_minute * ps_in_s) in
    let seconds' = I64.(of_int seconds * ps_in_s) in
    let ps = I64.(add (add hours' minutes') seconds') in
    Option.get @@ Ptime.Span.of_d_ps (days, ps)

  let to_string (x : t) : string =
    let if_z (v, suf) =
      if v = 0 then None else Some (Printf.sprintf "%d%s" v suf)
    in
    if Time.Relative.equal Time.Relative.zero x then "P0S"
    else
      let weeks, days, hours, minutes, seconds = split_units x in
      let big =
        List.filter_map if_z [ (weeks, "W"); (days, "D") ] |> String.concat ""
      in
      let small =
        List.filter_map if_z [ (hours, "H"); (minutes, "M"); (seconds, "S") ]
        |> String.concat ""
      in
      let part = "P" ^ big in
      if String.length small = 0 then part else part ^ "T" ^ small

  let of_string (s : string) : t =
    let open Angstrom in
    let number =
      take_while1 (function '0' .. '9' -> true | _ -> false) >>= fun x ->
      return (Some (int_of_string x))
    in
    let prefix = string "P" in
    let opt = option None in
    let sep = option () (char 'T' *> return ()) in
    let empty = end_of_input >>= fun () -> return Time.Relative.zero in
    let or_O = function Some x -> x | None -> 0 in
    let value =
      opt (number <* char 'W') >>= fun weeks ->
      opt (number <* char 'D') >>= fun days ->
      sep *> opt (number <* char 'H') >>= fun hours ->
      opt (number <* char 'M') >>= fun minutes ->
      opt (number <* char 'S') >>= fun seconds ->
      return
      @@ merge_unit (or_O weeks) (or_O days) (or_O hours) (or_O minutes)
           (or_O seconds)
    in
    let parser = prefix *> (value <|> empty) in
    s |> parse_string (*~consume:Consume.All*) parser |> function
    | Error e -> failwith e
    | Ok v -> v
end
