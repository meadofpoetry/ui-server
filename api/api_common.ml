open Containers

(** Type for responses returning a collection of ['a]
 ** [has_more] shows if there is some items left.
 ** A client may do a new request to pull remaining items in case if [has_more] is [true]
 ** [total] is a total number of items in a requested collection. Inserted to a response
 **  in case if a client added corresponding query in a request
 ** [decimation] is a flag indicating if decimation algorithm was applied to the data.
 ** Decimation is applied only if directly requested by the user
 **)
type 'a collection =
  { data       : 'a
  ; has_more   : (bool [@default false])
  ; total      : (int option [@default None])
  ; decimation : (bool [@default false]) (** TODO change to variant or string (algorithm type)? **)
  } [@@deriving yojson]

(** Scheme of a request **)
type scheme = [`WS | `REST]

(** Method of a request **)
type meth   = Cohttp.Code.meth

(** Path of a request **)
type path   = string list

let meth_of_uri (u:Uri.t) = match Uri.scheme u with
  | Some "ws" | Some "wss" -> `WS
  | _                      -> `REST

let (^::) = List.cons_maybe

module Domain = struct

  type path = string list

  module Query = struct

    include (Common.Uri.Query: module type of Common.Uri.Query)

    let ( >>* ) (u,x) f = match x with
      | Some x -> f (u,x)
      | None   -> u,None

    let from_query   = One (Common.Uri.Query.Time.from,Time)
    let till_query   = One (Common.Uri.Query.Time.till,Time)
    let limit_query  = One ("limit",Int)
    let total_query  = One ("total",Bool)
    let thin_query   = One ("thin",Bool)

    let get_time_query q =
      let r,q = get from_query q
                >>= fun (from,q) -> get till_query q
                >>| fun till     -> from,till
      in Result.map (fun (from,till) -> Common.Uri.Query.Time.Range.of_time ~from ~till) r,q

    let set_time_query (range:'a Common.Uri.Query.Time.Range.past) uri =
      let from_key = key_of_validation from_query in
      let till_key = key_of_validation till_query in
      Common.Uri.Query.Time.Range.add_to_uri ~from_key ~till_key (`Past range) uri

  end

end

module Device = struct
  include (Domain: module type of Domain with module Query := Domain.Query)

  let domain = "device"
  let port,state = "port","state"

  type req = [ `State
             | `Port of int * bool
             ]

  let eq = String.equal

  let req_to_path : req -> path = function
    | `Port (p,b) -> [port;string_of_int p;string_of_bool b]
    | `State      -> [state]
  let req_of_path : path -> req option = function
    | [x]     when eq x state -> Some `State
    | [x;y;z] when eq x port  -> Option.map2 (fun p b -> `Port (p,b))
                                             (int_of_string_opt y)
                                             (bool_of_string_opt z)
    | _                       -> None

  module Query = struct
    include Domain.Query

    let state_query =
      let f_of = Option.flat_map Common.Topology.state_of_string in
      let f_to = Fun.(Option.return % Common.Topology.state_to_string) in
      Filter ("state",Custom (f_of,f_to))

  end

end

type req = [ `Device  of Device.req ]
let req_to_path : req -> Domain.path = function
  | `Device x  -> Device.domain :: Device.req_to_path x
let req_of_path : string list -> req option = function
  | hd::tl when String.equal hd Device.domain  ->
     Option.map (fun x -> `Device x) @@ Device.req_of_path tl
  | _ -> None
