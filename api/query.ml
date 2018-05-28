open Common
open Containers

module Key = struct

  let equal = String.equal

  let from     = "from"
  let till     = "to"
  let limit    = "limit"
  let total    = "total"
  let decimate = "decimate"
  let filter   = "f"

end

(** Contains functions to deal with time queries in URI **)
module Time = struct

  (** Parsed time value from URI **)
  type t  = [ `Abs of Common.Time.t
            | `Rel of Common.Time.Relative.t
            ] [@@deriving eq]

  (** [to_abs t] converts [t] to absolute timestamp of type [Ptime.t]
   **)
  let to_abs : t -> Common.Time.t = function
    | `Rel x -> Common.Time.Relative.to_absolute x
    | `Abs x -> x

  (** [to_string t] converts [t] to string which can be inserted into URI query
   **)
  let to_string : t -> string = function
    | `Rel x -> Common.Time.Relative.to_string x
    | `Abs x -> Common.Time.to_rfc3339 x

  let of_string (s:string) : t option =
    let open Common.Time in
    let of_rfc3339 s  = of_rfc3339 s |> Result.map (fun (x,_,_) -> `Abs x) |> Result.to_opt in
    let of_int_s x    = Option.flat_map (fun x -> of_float_s @@ float_of_int x) @@ Int.of_string x
                        |> function Some x -> Some (`Abs x) | None -> None
    in
    let of_relative s = Relative.(of_string s |> Option.map (fun x -> `Rel x)) in
    let (>>=) x f = match x with Some x -> Some x | None -> f () in
    of_rfc3339 s >>= (fun () -> of_int_s s) >>= (fun () -> of_relative s)

  (** [of_uri ~key u] extracts value under [key] from the provided URI [u] and parses it.
   ** Possible time value formats are:
   ** - RFC3339
   ** - POSIX seconds
   ** - relative timestamp (now{+|-}{value}{unit} or plain 'now')
   **)
  let of_uri ~(key:string) (uri:Uri.t) : t option =
    match Uri.get_query_param uri key with
    | None   -> None
    | Some s -> of_string s

  module Range = struct

    type time = t

    (** Abstract time range type **)
    type 'a _t = [ `Range of 'a * 'a
                 | `From  of 'a
                 | `Till  of 'a
                 | `Whole
                 | `Now
                 ]
    (** Time range, containing absolute or relative timestamps **)
    type t     = time _t
    (** Time range, containing only absolute timestamps **)
    type t_abs = Common.Time.t _t

    (** [add_to_uri ?fk ?tk t u] functionally updates URI [u] with time range [t].
     ** Queries are inserted under provided [fk] and [tk] keys.
     **)
    let add_to_uri ?(from_key=Key.from) ?(till_key=Key.till) (range:t) uri =
      let ins k v u = match v with Some x -> Uri.add_query_param' u (k,to_string x)
                                 | None -> u
      in
      let f,t = match range with
        | `Whole       -> None,None
        | `Now         -> Some (`Rel `Now), Some (`Rel `Now)
        | `Till x      -> None, Some x
        | `From x      -> Some x, None
        | `Range (f,t) -> Some f, Some t
      in ins from_key f uri |> ins till_key t

    let of_time ~(from:time option) ~(till:time option) : t =
      let open Common.Time in
      match from,till with
      | Some (`Rel `Now), Some (`Rel `Now) -> `Now
      | Some x, None   -> `From x
      | None  , Some x -> `Till x
      | Some f, Some t -> `Range (f,t)
      | None  , None   -> `Whole

    (** [of_uri ?fk ?tk u] extracts time range from URI [u].
     ** Search of time values is performed under provided [fk] and [tk] keys.
     **)
    let of_uri ?(from_key=Key.from) ?(till_key=Key.till) uri : t =
      let open Common.Time in
      of_time ~from:(of_uri ~key:from_key uri) ~till:(of_uri ~key:till_key uri)

    (** [to_abs t] converts time range [t] to time range with absolute timestamps
     **)
    let to_abs : t -> t_abs = function
      | (`Whole | `Now) as x -> x
      | `From x              -> `From (to_abs x)
      | `Till x              -> `Till (to_abs x)
      | `Range (f,t)         -> `Range (to_abs f, to_abs t)

  end

end

module Filter = struct

  (** Filter query type **)
  type t =
    { key   : string
    ; items : string list
    }

  let key_of_string ?(key=Key.filter) (s:string) : (string,string) result =
    let open Angstrom in
    let pre = string key in
    let opn = char '[' in
    let cls = char ']' in
    let str = take_while1 (function ']' -> false | _ -> true) in
    let parser = (pre *> opn *> str) >>= fun s -> cls >>| fun _ -> s
    in parse_string parser s

  let of_query ?(key=Key.filter) (q:string * string list) : t option =
    match key_of_string ~key @@ fst q with
    | Error _ -> None
    | Ok k    -> Some { key = k; items = snd q }

  let of_uri ?(key=Key.filter) (u:Uri.t) : t list =
    List.filter_map (of_query ~key) @@ Uri.query u

end

module Collection = struct

  type raw = [ `From     of Time.t
             | `Till     of Time.t
             | `Filter   of Filter.t
             | `Total    of bool
             | `Decimate of bool
             | `Limit    of int
             ]

  type t =
    { time     : Time.Range.t
    ; filter   : Filter.t list
    ; total    : bool
    ; limit    : int option
    ; decimate : bool
    }

  let of_query (q:string * string list) : raw option =
    let open Key in
    let map = Option.map in
    match q with
    | k,[v] when equal k from     -> map (fun x -> `From x) @@ Time.of_string v
    | k,[v] when equal k till     -> map (fun x -> `Till x) @@ Time.of_string v
    | k,[v] when equal k total    -> map (fun x -> `Total x) @@ bool_of_string_opt v
    | k,[v] when equal k limit    -> map (fun x -> `Limit x) @@ int_of_string_opt v
    | k,[v] when equal k decimate -> map (fun x -> `Decimate x) @@ bool_of_string_opt v
    | x -> Option.map (fun x -> `Filter x) @@ Filter.of_query x

  let of_uri (uri:Uri.t) : (t,string list) result =
    let query = Uri.query uri in
    let good,bad = List.fold_left (fun (g,b) x -> match of_query x with
                                                  | Some x -> x :: g, b
                                                  | None   -> g, fst x :: b) ([],[]) query
    in
    if not @@ List.is_empty bad
    then Error bad
    else Ok { filter   = List.filter_map (function `Filter x -> Some x | _ -> None) good
            ; time     = (let from = List.find_map (function `From x -> Some x | _ -> None) good in
                          let till = List.find_map (function `Till x -> Some x | _ -> None) good in
                          Time.Range.of_time ~from ~till)
            ; total    = List.find_map (function `Total x -> Some x | _ -> None) good
                         |> Option.get_or ~default:false
            ; decimate = List.find_map (function `Decimate x -> Some x | _ -> None) good
                         |> Option.get_or ~default:false
            ; limit    = List.find_map (function `Limit x -> Some x | _ -> None) good
            }

  let validate (uri:Uri.t) =
    let query = Uri.query uri in
    let keys = List.map fst query in
    keys

end
