open Common
open Containers

module Raw = struct

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

  module Range = struct

    type time = t

    type 'a past = [ `Range of 'a * 'a
                   | `From  of 'a
                   | `Till  of 'a
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
        | `Now    -> None, None
        | `Past x -> (match x with
                      | `Till x      -> None, Some x
                      | `From x      -> Some x, None
                      | `Range (f,t) -> Some f, Some t)
      in ins from_key f uri |> ins till_key t

    let of_time ~(from:time option) ~(till:time option) : t =
      let open Common.Time in
      match from,till with
      | None  , None   -> `Now
      | Some x, None   -> `Past (`From x)
      | None  , Some x -> `Past (`Till x)
      | Some f, Some t -> `Past (`Range (f,t))

    (** [to_abs t] converts time range [t] to time range with absolute timestamps
     **)
    let to_abs : t -> t_abs = function
      | `Now                 -> `Now
      | `Past (`From x)      -> `Past (`From (to_abs x))
      | `Past (`Till x)      -> `Past (`Till (to_abs x))
      | `Past (`Range (f,t)) -> `Past (`Range (to_abs f, to_abs t))

  end

end

module Filter = struct

  let key = "f"

  (** Filter query type **)
  type t = Raw.Key.t * string list

  let name_of_key ?(base_key=key) (k:Raw.Key.t) : (string,string) result =
    let open Angstrom in
    let pre = string base_key in
    let opn = char '[' in
    let cls = char ']' in
    let str = take_while1 (function ']' -> false | _ -> true) in
    let parser = (pre *> opn *> str) >>= fun s -> cls >>| fun _ -> s
    in parse_string parser k

  let name_to_key ?(base_key=key) name = Printf.sprintf "%s[%s]" base_key name

end

module Validation = struct

  type _ v_simple =
    | Bool   : bool v_simple
    | Int    : int v_simple
    | Time   : Time.t v_simple
    | Custom : (string option -> 'a option) * ('a -> string option) -> 'a v_simple

  type _ v =
    | List       : string * 'a v_simple -> 'a list v              (* key and item validation *)
    | Filter     : string * 'a v_simple -> 'a list v              (* name and item validation *)
    | Filter_one : string * 'a v_simple -> 'a v
    | One        : string * 'a v_simple -> 'a v                   (* key and item validation *)

  type err = [ `Bad_value of Raw.t
             | `Unknown   of Raw.t list
             ] [@@deriving yojson]

  let validate_simple : type a. a v_simple -> Raw.Value.t -> (a,Raw.Value.t) result = fun validation query ->
    let to_res = function Some x -> Ok x | None -> Error query in
    match validation,query with
    | Bool,[v]         -> bool_of_string_opt v |> to_res
    | Int,[v]          -> int_of_string_opt v  |> to_res
    | Time,[v]         -> Time.of_string v     |> to_res
    | Custom (f,_),[ ] -> f None               |> to_res
    | Custom (f,_),[v] -> f (Some v)           |> to_res
    | _                -> Error query

  let simple_to_value : type a. a -> a v_simple -> Raw.Value.t = fun v validation ->
    match validation with
    | Bool         -> List.pure @@ string_of_bool v
    | Int          -> List.pure @@ string_of_int v
    | Time         -> List.pure @@ Time.to_string v
    | Custom (_,f) -> match f v with Some v -> List.pure v | None -> [ ]

  let remove key query = List.Assoc.remove ~eq:Raw.Key.equal key query

  type err_ext = [ `Not_found of Raw.Key.t | err ]

  let key_of_validation : type a. a v -> string = function
    | List (k,_)       -> k
    | Filter (n,_)     -> Filter.name_to_key n
    | Filter_one (n,_) -> Filter.name_to_key n
    | One (k,_)        -> k

  let validate : type a. Raw.t list -> a v -> ((Raw.Key.t * a),err_ext) result = fun q validation ->
    let open Result.Infix in
    let find k q    = List.Assoc.get ~eq:Raw.Key.equal k q in
    let ( >>* ) k f = match find k q with Some q -> f q | None -> Error (`Not_found k) in
    let ( %> )      = Fun.( %> ) in
    let of_rlist k  = List.fold_left (fun (err,acc) x -> match x with Error e -> e :: err,acc
                                                                    | Ok x    -> err,x ::acc)
                                     ([],[])
                      %> (function [],acc -> Ok acc | err,_ -> Error (`Bad_value (k,List.concat err)))
    in
    let k = key_of_validation validation in
    match validation with
    | List (_,x)       ->
       k >>* ((List.map (fun q -> validate_simple x [q])) %> of_rlist k) >|= Pair.make k
    | Filter (_,x)     ->
       k >>* (List.map (fun q -> validate_simple x [q]) %> of_rlist k)
       >|= Pair.make k
    | Filter_one (_,x) ->
       k >>* (fun v -> validate_simple x v
                       |> Result.map_err (fun e -> `Bad_value (k,e)))
       >|= Pair.make k
    | One (_,x)        ->
       k >>* (fun v -> validate_simple x v |> Result.map_err (fun e -> `Bad_value (k,e)))
       >|= Pair.make k

  type 'a t = ('a,err) result

  let get : type a. a v -> Raw.t list -> a option t * Raw.t list =
    fun validation query ->
    match validate query validation with
    | Ok (k,q)             -> Ok (Some q),List.Assoc.remove ~eq:Raw.Key.equal k query
    | Error (`Not_found _) -> Ok None,query
    | Error (`Unknown _ | `Bad_value _) as e -> e,query

  let get_or : type a. default:a -> a v -> Raw.t list -> a t * Raw.t list =
    fun ~default v q -> get v q |> fun (r,q)-> (Result.map (Option.get_or ~default) r),q

  let last_or_err : 'a t * Raw.t list -> 'a t = function
    | (Ok v),[]   -> Ok v
    | (Ok _),l    -> Error (`Unknown l)
    | (Error e),q -> Error e

  let next (f:'a * Raw.t list -> 'b t * Raw.t list) : 'a t * Raw.t list -> 'b t * Raw.t list = function
    | (Ok v),q    -> f (v,q)
    | (Error e),q -> Error e,q
  let map_last_or_err (f:'a -> 'b) (x:'a t * Raw.t list) : 'b t * Raw.t list =
    (Result.map f @@ last_or_err x),snd x

  let ( >>= ) x f = next f x
  let ( >>| ) x f = map_last_or_err f x

end
