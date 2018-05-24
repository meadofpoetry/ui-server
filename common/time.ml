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

module RFC3339 = struct
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

module Useconds = struct
  type t = Ptime.t

  let of_useconds s =
    let s = Int64.(s / 1000000L |> to_int) in
    Ptime.add_span Ptime.epoch (Ptime.Span.of_int_s s)

  let to_useconds v =
    let s = Option.get_exn @@ Ptime.Span.to_int_s @@ Ptime.to_span v in
    Int64.(of_int s * 1000000L)

  let of_yojson = function
    | `Int x    -> begin
        Int64.of_int x
        |> of_useconds
        |> function Some v -> Ok v | None -> Error (Printf.sprintf "Useconds.of_yojson: bad input int %d" x)
      end
    | `Intlit x -> begin
        Option.(Int64.of_string x >>= of_useconds)
        |> function Some v -> Ok v | None -> Error ("Useconds.of_yojson: bad input intlit " ^ x)
      end
    | js -> Error ("Useconds.of_yojson: bad input " ^ (Yojson.Safe.pretty_to_string js))

  let to_yojson v = `Intlit (Int64.to_string @@ to_useconds v)

end
               
module Seconds = struct
  type t = Ptime.t

  let equal = Ptime.equal
         
  let of_seconds s =
    Ptime.add_span Ptime.epoch (Ptime.Span.of_int_s s)

  let of_seconds64 s =
    Int64.to_int s |> of_seconds

  let to_seconds v = Option.get_exn @@ Ptime.Span.to_int_s @@ Ptime.to_span v

  let to_seconds64 v = Int64.of_int @@ to_seconds v

  let to_string v =
    to_seconds v |> string_of_int

  let of_string_opt s =
    Option.(int_of_string_opt s >>= of_seconds)

  let of_string s = Option.get_exn @@ of_string_opt s
                  
  let of_yojson = function
    | `Int x -> begin match of_seconds x with
                | Some v -> Ok v
                | None   -> Error "Seconds.of_yojson: bad input"
                end
    | _ -> Error "Seconds.of_yojson: bad input"

  let to_yojson v = `Int (to_seconds v)

end

module Hours = struct
  type t = Ptime.t
         
  let of_hours s = Ptime.add_span Ptime.epoch (Ptime.Span.of_int_s (3600 * s))

  let to_hours v = (Option.get_exn @@ Ptime.Span.to_int_s @@ Ptime.to_span v) / 3600

  let to_string v =
    to_hours v |> string_of_int

  let of_string_opt s =
    Option.(int_of_string_opt s >>= of_hours)

  let of_string s = Option.get_exn @@ of_string_opt s
                  
  let of_yojson = function
    | `Int x -> begin match of_hours x with
                | Some v -> Ok v
                | None   -> Error "Hours.of_yojson: bad input"
                end
    | _ -> Error "Hours.of_yojson: bad input"

  let to_yojson v = `Int (to_hours v)

end

module Period = struct

  module Hours = struct

    type t = Ptime.span

    let of_hours s = Ptime.Span.of_int_s (3600 * s)

    let to_hours v = (Option.get_exn @@ Ptime.Span.to_int_s v) / 3600

    let to_string v =
      to_hours v |> string_of_int

    let of_string_opt s =
      Option.(int_of_string_opt s >|= of_hours)

    let of_string s = Option.get_exn @@ of_string_opt s

    let of_yojson = function
      | `Int x -> Ok (of_hours x)
      | _ -> Error "Hours.of_yojson: bad input"

    let to_yojson v = `Int (to_hours v)

  end

end

module Interval = struct

  type timestamp = t [@@deriving yojson]

  type t = timestamp * timestamp [@@deriving yojson]

  let from (t:t) = fst t
  let till (t:t) = snd t

end

module Relative : sig

  (** Possible format:
   ** now{+|-}{value}{unit} or plain 'now'
   ** Possible time units are:
   ** 's'   - second
   ** 'min' - minute
   ** 'h'   - hour
   ** 'd'   - day
   ** 'wk'  - week
   **)

  type t = [ `Now         (** no offset **)
           | `Sec  of int
           | `Min  of int
           | `Hour of int
           | `Day  of int
           | `Week of int
           ]

  val to_absolute : t -> Ptime.t
  val to_span     : t -> Ptime.span
  val of_span     : Ptime.span -> t
  val to_string   : t -> string
  val of_string   : string -> t option

end = struct

  let s_in_minute = 60
  let s_in_hour   = s_in_minute * 60
  let s_in_day    = s_in_hour * 24
  let s_in_week   = s_in_day * 7

  type t = [ `Now | `Sec of int | `Min of int | `Hour of int | `Day of int | `Week of int ]

  let to_value : t -> int = function
    | `Now -> 0 | (`Sec x | `Min x | `Hour x | `Day x | `Week x) -> x

  let to_seconds : t -> int = function
    | `Now    -> 0
    | `Sec d  -> d
    | `Min d  -> d * s_in_minute
    | `Hour d -> d * s_in_hour
    | `Day d  -> d * s_in_day
    | `Week d -> d * s_in_week

  let divisors = [
      (max_int, `Wk);  (* many wk = many wk *)
      (7,  `D);        (* 7 d = 1 wk *)
      (24, `Hr);       (* 24 hr = 1 d *)
      (60, `Min);      (* 60 min = 1 hr *)
      (60, `Sec)       (* 60 sec = 1 min *)
    ]

  let compute_duration secs =
    let rec aux remain res = function
      | [] -> res
      | (n, s) :: ds -> aux (remain / n) ((remain mod n, s) :: res) ds
    in
    aux secs [] (List.rev divisors)

  let to_max_unit (secs:int) : t =
    compute_duration secs
    |> List.filter_map (fun (d,s) -> if d <> 0 then Some s else None)
    |> List.rev
    |> function
      | [ ]  -> `Sec 0
      | x::_ -> (match x with
                 | `Wk   -> `Week (secs / s_in_week)
                 | `D    -> `Day (secs / s_in_day)
                 | `Hr   -> `Hour (secs / s_in_hour)
                 | `Min  -> `Min (secs / s_in_minute)
                 | `Sec  -> `Sec secs)

  let to_unit_string : t -> string = function
    | `Now    -> ""  | `Sec _ -> "s" | `Min _  -> "min"
    | `Hour _ -> "h" | `Day _ -> "d" | `Week _ -> "wk"
  let of_unit_string (d:int) : string -> t option = function
    | "s"   -> Some (`Sec d)
    | "min" -> Some (`Min d)
    | "h"   -> Some (`Hour d)
    | "d"   -> Some (`Day d)
    | "wk"  -> Some (`Week d)
    | _     -> None

  let of_span (s:Ptime.span) : t =
    to_max_unit @@ int_of_float @@ Ptime.Span.to_float_s s
  let to_span (t:t) =
    Ptime.Span.of_int_s @@ to_seconds t

  let to_absolute (t:t) : Ptime.t =
    let span = to_span t in
    let now = Option.get_exn @@ Ptime.of_float_s @@ Unix.gettimeofday () in
    Option.get_exn @@ add_span now span

  let to_string (t:t) = Printf.sprintf "now%+d%s" (to_value t) (to_unit_string t)
  let of_string (s:string) : t option =
    let open Angstrom in
    let sub,add = char '-',char '+' in
    let str     = take_while1 (function 'a'..'z' -> true | _ -> false) in
    let number  = take_while1 (function '0'..'9' -> true | _ -> false) in
    let prefix = string "now" in
    let empty  = end_of_input >>= fun () -> return `Now in
    let value  =
      (sub <|> add)
      >>= fun sign -> number
      >>= fun num  -> return (int_of_string @@ String.of_char sign ^ num)
      >>= fun v    -> str
      >>= fun s    -> (match of_unit_string v s with
                       | Some u -> return u
                       | None   -> fail "bad unit value")
    in
    let parser = prefix *> (value <|> empty) in
    parse_string parser s |> Result.to_opt

end
