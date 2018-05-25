open Containers

(** Contains functions to deal with time queries in URI **)
module Time = struct

  (** Default URI time queries keys **)
  let from_key,till_key = "from","to"

  (** Parsed time value from URI **)
  type t  = [ `Abs of Common.Time.t
            | `Rel of Common.Time.Relative.t
            ]

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

  (** [of_uri ~key u] extracts value under [key] from the provided URI [u] and parses it.
   ** Possible time value formats are:
   ** - RFC3339
   ** - POSIX seconds
   ** - relative timestamp (now{+|-}{value}{unit} or plain 'now')
   **)
  let of_uri ~(key:string) (uri:Uri.t) : t option =
    let open Common.Time in
    let of_rfc3339 s  = of_rfc3339 s |> Result.map (fun (x,_,_) -> `Abs x) |> Result.to_opt in
    let of_int_s x    = Option.flat_map (fun x -> of_float_s @@ float_of_int x) @@ Int.of_string x
                        |> function Some x -> Some (`Abs x) | None -> None
    in
    let of_relative s = Relative.(of_string s |> Option.map (fun x -> `Rel x)) in
    match Uri.get_query_param uri key with
    | None   -> None
    | Some s -> let (>>=) x f = match x with Some x -> Some x | None -> f () in
                of_rfc3339 s >>= (fun () -> of_int_s s) >>= (fun () -> of_relative s)

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
    let add_to_uri ?(from_key=from_key) ?(till_key=till_key) (range:t) uri =
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

    (** [of_uri ?fk ?tk u] extracts time range from URI [u].
     ** Search of time values is performed under provided [fk] and [tk] keys.
     **)
    let of_uri ?(from_key=from_key) ?(till_key=till_key) uri : t =
      let open Common.Time in
      match of_uri ~key:from_key uri, of_uri ~key:till_key uri with
      | Some (`Rel `Now), Some (`Rel `Now) -> `Now
      | Some x, None   -> `From x
      | None  , Some x -> `Till x
      | Some f, Some t -> `Range (f,t)
      | None  , None   -> `Whole

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

  let prefix = "filter"

  (** Filter query type **)
  type t =
    { key   : string
    ; items : string list
    }

  let key_of_string (s:string) =
    let open Angstrom in
    let pre = string prefix in
    let opn = char '[' in
    let cls = char ']' in
    let str = take_while1 (function ']' -> false | _ -> true) in
    let parser = (pre *> opn *> str) >>= fun s -> cls >>| fun _ -> s
    in parse_string parser s

end

let limit_key = "limit"
let total_key = "total"

(** Basic query type **)
type t  =
  { time     : Time.Range.t_abs
  ; filters  : Filter.t list
  ; limit    : int option
  ; total    : bool
  ; decimate : bool (* FIXME change to variant (algorithm type)? *)
  }

(* let of_uri (uri:Uri.t) =
 *   let query = Uri.query uri in
 *   let keys  = List.map fst query in
 *   let preds = [ String.equal Time.from_key
 *               ; String.equal Time.till_key
 *               ; String.equal limit_key
 *               ; String.equal total_key
 *               ; Fun.(Filter.key_of_string %> Result.is_ok)
 *               ]
 *   in
 *   let rec check preds k = match preds with
 *     | []     -> Some k
 *     | fn::tl -> if fn k then None else check tl k
 *   in
 *   let dup,bad = List.fold_filter_map (fun (prev,dup) k ->
 *                     let acc = if List.mem ~eq:String.equal k prev
 *                               then k::prev,k::dup else k::prev,dup
 *                     in acc,check preds k) keys
 *   in match bad with
 *      | [] -> Ok 1
 *      | ks -> Error (Printf.sprintf "Bad keys: %s" @@ String.concat ", " ks) *)
