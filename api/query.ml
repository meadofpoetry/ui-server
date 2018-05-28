open Common
open Containers

module Base = struct

  module Key = struct

    type t = string [@@deriving yojson]
    let equal   = String.equal
    let compare = String.compare

  end

  module Value = struct

    type t = string list [@@deriving yojson]
    let compare (t1:t) (t2:t) =
      List.compare String.compare t1 t2
    let equal (t1:t) (t2:t) =
      let t1 = List.sort String.compare t1 in
      let t2 = List.sort String.compare t2 in
      (Equal.list String.equal) t1 t2

  end

  type t = Key.t * Value.t [@@deriving yojson]

  let equal (t1:t) (t2:t) =
    if Key.equal (fst t1) (fst t2)
    then Value.equal (snd t1) (snd t2)
    else false

  let compare (t1:t) (t2:t) =
    match Key.compare (fst t1) (fst t2) with
    | 0 -> Value.compare (snd t1) (snd t2)
    | x -> x

  let to_string (x:t) = Uri.encoded_of_query [x] |> Uri.pct_decode

end

(** Contains functions to deal with time queries in URI **)
module Time = struct

  let from     = "from"
  let till     = "to"

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

  let of_query ~(key:string) (q:Base.t) : t option =
    match Base.Key.equal key (fst q), snd q with
    | true,[v] -> of_string v
    | _        -> None

  let from_of_query ?(key=from) (q:Base.t) : t option = of_query ~key q
  let till_of_query ?(key=till) (q:Base.t) : t option = of_query ~key q

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

    type 'a past = [ `Range of 'a * 'a
                   | `From  of 'a
                   | `Till  of 'a
                   | `Whole
                   ]

    (** Abstract time range type **)
    type 'a _t = [ `Now
                 | `Past of 'a past
                 ]

    (** Time range, containing absolute or relative timestamps **)
    type t     = time _t
    (** Time range, containing only absolute timestamps **)
    type t_abs = Common.Time.t _t

    (** [add_to_uri ?fk ?tk t u] functionally updates URI [u] with time range [t].
     ** Queries are inserted under provided [fk] and [tk] keys.
     **)
    let add_to_uri ?(from_key=from) ?(till_key=till) (range:t) uri =
      let ins k v u = match v with Some x -> Uri.add_query_param' u (k,to_string x)
                                 | None -> u
      in
      let f,t = match range with
        | `Now    -> Some (`Rel `Now), Some (`Rel `Now)
        | `Past x -> (match x with
                      | `Whole       -> None,None
                      | `Till x      -> None, Some x
                      | `From x      -> Some x, None
                      | `Range (f,t) -> Some f, Some t)
      in ins from_key f uri |> ins till_key t

    let of_time ~(from:time option) ~(till:time option) : t =
      let open Common.Time in
      match from,till with
      | Some (`Rel `Now), Some (`Rel `Now) -> `Now
      | Some x, None   -> `Past (`From x)
      | None  , Some x -> `Past (`Till x)
      | Some f, Some t -> `Past (`Range (f,t))
      | None  , None   -> `Past `Whole

    (** [of_uri ?fk ?tk u] extracts time range from URI [u].
     ** Search of time values is performed under provided [fk] and [tk] keys.
     **)
    let of_uri ?(from_key=from) ?(till_key=till) uri : t =
      let open Common.Time in
      of_time ~from:(of_uri ~key:from_key uri) ~till:(of_uri ~key:till_key uri)

    (** [to_abs t] converts time range [t] to time range with absolute timestamps
     **)
    let to_abs : t -> t_abs = function
      | (`Past `Whole | `Now) as x -> x
      | `Past (`From x)      -> `Past (`From (to_abs x))
      | `Past (`Till x)      -> `Past (`Till (to_abs x))
      | `Past (`Range (f,t)) -> `Past (`Range (to_abs f, to_abs t))

  end

end

module Filter = struct

  let key = "f"

  (** Filter query type **)
  type t = Base.Key.t * string list

  let name_of_key ?(base_key=key) (k:Base.Key.t) : (string,string) result =
    let open Angstrom in
    let pre = string base_key in
    let opn = char '[' in
    let cls = char ']' in
    let str = take_while1 (function ']' -> false | _ -> true) in
    let parser = (pre *> opn *> str) >>= fun s -> cls >>| fun _ -> s
    in parse_string parser k

  let name_to_key ?(base_key=key) name = Printf.sprintf "%s[%s]" base_key name

  let of_query ?(key=key) ?name (q:Base.t) : t option =
    match name, name_of_key (fst q) with
    | Some n, Ok k when String.equal k n -> Some (n,snd q)
    | None, Ok k -> Some (k,snd q)
    | _          -> None

  let of_queries ?(key=key) ?name (q:Base.t list) : t option =
    List.find_map (of_query ~key ?name) q

  let of_uri ?(key=key) ~name (u:Uri.t) : t option =
    List.find_map (of_query ~key ?name) @@ Uri.query u

  let of_uri_all ?(key=key) (u:Uri.t) : t list =
    List.filter_map (of_query ~key) @@ Uri.query u

end

module Make_simple(M:sig type t val of_string : string -> t option end) = struct

  let of_query ~key (q:Base.t) : 'a option =
    match Base.Key.equal key (fst q), (snd q) with
    | true,[v] -> M.of_string v
    | _        -> None

  let of_uri ~key (u:Uri.t) : 'a option =
    Option.flat_map M.of_string @@ Uri.get_query_param u key

end

module Simple = struct

  module Bool  = Make_simple(struct type t = bool let of_string = bool_of_string_opt end)
  module Int   = Make_simple(Int)
  module Int32 = Make_simple(Int32)
  module Int64 = Make_simple(Int64)

end

module Validation = struct

  type _ simple =
    | Bool   : bool simple
    | Int    : int simple
    | Int32  : int32 simple
    | Int64  : int64 simple
    | Key    : string list -> string simple
    | Empty  : unit simple
    | Time   : Time.t simple
    | Custom : (string option -> 'a option) -> 'a simple

  type _ t =
    | List   : string * 'a simple -> 'a list t              (* key and item validation *)
    | Filter : string * 'a simple -> 'a list t              (* name and item validation *)
    | One    : string * 'a simple -> 'a t                   (* key and item validation *)
    | Keys   : string * string list -> string list t        (* key and possible values *)
    | Custom : string * (Base.t -> 'a option) -> 'a t

  let validate_simple : type a. a simple -> string list -> a option = fun validation query ->
    match validation,query with
    | Bool,[v]     -> bool_of_string_opt v
    | Int,[v]      -> int_of_string_opt v
    | Int32,[v]    -> Int32.of_string_opt v
    | Int64,[v]    -> Int64.of_string_opt v
    | Key ks,[v]   -> if List.mem ~eq:String.equal v ks then Some v else None
    | Empty,[ ]    -> Some ()
    | Time,[v]     -> Time.of_string v
    | Custom f,[ ] -> f None
    | Custom f,[v] -> f (Some v)
    | _            -> None

  let find k q = List.Assoc.get ~eq:Base.Key.equal k q

  let rec validate : type a. Base.t list -> a t -> (Base.Key.t * a) option = fun q validation ->
    let open Option.Infix in
    match validation with
    | List (k,x)   -> find k q
                      >|= (List.map (fun q -> validate_simple x [q]))
                      >>= List.all_some
                      >|= Pair.make k
    | Filter (n,x) -> Filter.of_queries ~name:n q
                      >|= snd
                      >|= (List.map (fun q -> validate_simple x [q]))
                      >>= List.all_some
                      >|= Pair.make (Filter.name_to_key n)
    | One (k,x)    -> find k q
                      >>= validate_simple x
                      >|= Pair.make k
    | Keys (k,ks)  -> find k q
                      >|= (List.partition (fun x -> List.mem ~eq:String.equal x ks))
                      >>= (function ks,[] -> Some ks | _ -> None)
                      >|= Pair.make k
    | Custom (k,f) -> find k q
                      >|= (Pair.make k)
                      >>= f
                      >|= Pair.make k

  let get : type a. a t -> Base.t list -> a option * Base.t list =
    fun validation query ->
    match validate query validation with
    | Some (key,q) -> Some q,List.Assoc.remove ~eq:Base.Key.equal key query
    | None         -> None,query

end
