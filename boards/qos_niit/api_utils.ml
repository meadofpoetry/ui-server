open Containers
open Common.Uri.Query

let (^::) = List.cons_maybe

type stream = [ `TS   of Common.Stream.id option
              | `T2MI of int option
              ]

type err = Bad_query of Validation.err
         | Other     of string [@@deriving yojson]

let eq = String.equal

let ts,t2mi = "ts","t2mi"

let stream_to_path : stream -> string list = function
  | `TS x   ->
     let sid = Option.map Fun.(Common.Stream.id_to_int32 %> Int32.to_string) x in
     ts :: (sid ^:: [])
  | `T2MI x ->
     let sid = Option.map Int.to_string x in
     ts :: (sid ^:: [])
let stream_of_path : string list -> stream option = function
  | [x] when eq x ts   -> Some (`TS None)
  | [x] when eq x t2mi -> Some (`T2MI None)
  | [x;y] when eq x ts ->
     Option.map (fun i -> `TS (Some (Common.Stream.id_of_int32 i))) @@ Int32.of_string y
  | [x;y] when eq x t2mi ->
     Option.map (fun i -> `T2MI (Some i)) @@ Int.of_string y
  | _ -> None

module Domain = struct

  let equal = String.equal
  type path = string list

  open Validation

  let from_query   = One ("from",Time)
  let till_query   = One ("to",Time)
  let limit_query  = One ("limit",Int)
  let total_query  = One ("total",Bool)
  let thin_query   = One ("thin",Bool)

  let get_time_query q =
    let r,q = get from_query q
              >>= fun (from,q) -> get till_query q
              >>| fun till     -> from,till
    in Result.map (fun (from,till) -> Time.Range.of_time ~from ~till) r,q
  let get_thin_query  q = get_or ~default:false thin_query q
  let get_total_query q = get_or ~default:false total_query q
  let get_limit_query q = get limit_query q

  let set_time_query (range:'a Time.Range.past) uri =
    let from_key = key_of_validation from_query in
    let till_key = key_of_validation till_query in
    Time.Range.add_to_uri ~from_key ~till_key (`Past range) uri
  let set_thin_query  v uri = insert thin_query v uri
  let set_total_query v uri = insert total_query v uri
  let set_limit_query v uri = insert limit_query v uri
end

module Device = struct
  include Domain

  let domain = "device"
  let errors,mode,info,
      port,state,status,
      reset = "errors","mode","info","port",
              "state","status","reset"

  type mode = [ `T2MI | `JITTER ]
  type req  = [ `Errors
              | `Info
              | `Mode of mode
              | `Port of int * bool
              | `State
              | `Status
              | `Reset
              ]

  let eq = equal
  let mode_to_string = function `T2MI -> "t2mi" | `JITTER -> "jitter"
  let mode_of_string = function
    | x when eq x "t2mi"   -> Some `T2MI
    | x when eq x "jitter" -> Some `JITTER
    | _                    -> None

  let req_to_path : req -> path = function
    | `Errors     -> [errors]
    | `Info       -> [info]
    | `Mode x     -> [mode;mode_to_string x]
    | `Port (p,b) -> [port;string_of_int p;string_of_bool b]
    | `State      -> [state]
    | `Status     -> [status]
    | `Reset      -> [reset]
  let req_of_path : path -> req option = function
    | [x] when eq x errors -> Some `Errors
    | [x] when eq x info   -> Some `Info
    | [x] when eq x state  -> Some `State
    | [x] when eq x status -> Some `Status
    | [x;y] when eq x mode ->
       Option.map (fun x -> `Mode x) @@ mode_of_string y
    | [x;y;z] when eq x port ->
       Option.map2 (fun p b -> `Port (p,b)) (int_of_string_opt y) (bool_of_string_opt z)
    | _-> None

  open Validation

  let state_query =
    let f_of = Option.flat_map Common.Topology.state_of_string in
    let f_to = Fun.(Option.return % Common.Topology.state_to_string) in
    Filter ("state",Custom (f_of,f_to))
  let errors_query = Filter ("errors",Int)

  let get_state_query q =
    let r,q  = get state_query q in
    Result.map (Option.map (List.sort_uniq ~cmp:Common.Topology.compare_state)) r,q
  let get_errors_query q = get errors_query q

  let set_state_query v uri  = insert state_query v uri
  let set_errors_query v uri = insert errors_query v uri

end


module Errors = struct
  include Domain
  let domain  = "errors"
  let percent,has_any = "percent","has-any"

  type req = [ `Errors  of stream
             | `Percent of stream
             | `Has_any of stream
             ]

  let eq = Domain.equal

  let req_to_path : req -> string list = function
    | `Errors x  -> stream_to_path x
    | `Percent x -> percent :: stream_to_path x
    | `Has_any x -> has_any :: stream_to_path x
  let req_of_path : string list -> req option = function
    | hd::tl when eq hd ts || eq hd t2mi ->
       Option.map (fun x -> `Errors x) @@ stream_of_path (hd::tl)
    | hd::tl when eq hd percent ->
       Option.map (fun x -> `Percent x) @@ stream_of_path tl
    | hd::tl when eq hd has_any ->
       Option.map (fun x -> `Has_any x) @@ stream_of_path tl
    | _ -> None

  open Validation

  let errors_query = Filter ("errors",Int)
  let level_query  = Filter ("level",Int)

  let get_level_query  q = get level_query q
  let get_errors_query q = get errors_query q

  let set_level_query v uri  = insert level_query v uri
  let set_errors_query v uri = insert errors_query v uri

end


module Streams = struct
  include Domain
  let domain = "streams"
  let ts,t2mi = "ts","t2mi"
  let state,structure,bitrate,
      sequence,section = "state","structure","bitrate",
                         "sequence","section"

  type req = [ `Streams   of Common.Stream.id option
             | `State     of stream
             | `Structure of stream
             | `Bitrate   of Common.Stream.id option
             | `Sequence  of int option
             | `Section   of Common.Stream.id * int
             ]

  let eq = Domain.equal

  let req_to_path : req -> path = function
    | `Streams id ->
       let sid = Option.map Fun.(Common.Stream.id_to_int32 %> Int32.to_string) id in sid ^:: []
    | `State x          -> state :: stream_to_path x
    | `Structure x      -> structure :: stream_to_path x
    | `Bitrate x        -> bitrate :: stream_to_path (`TS x)
    | `Sequence x       -> section :: stream_to_path (`T2MI x)
    | `Section (sid,id) ->
       let ts = stream_to_path (`TS (Some sid)) in
       let id = Int.to_string id in
       (section :: ts) @ (id :: [])
  let req_of_path : path -> req option = function
    | [ ] -> Some (`Streams None)
    | [x] -> Option.map (fun x -> `Streams (Some (Common.Stream.id_of_int32 x))) @@ Int32.of_string x
    | hd::tl when eq hd state ->
       Option.map (fun x -> `State x) @@ stream_of_path tl
    | hd::tl when eq hd structure ->
       Option.map (fun x -> `Structure x) @@ stream_of_path tl
    | hd::tl when eq hd bitrate ->
       Option.flat_map (function `TS id -> Some (`Bitrate id) | _ -> None)
       @@ stream_of_path tl
    | [x;y]   when eq x sequence && eq y t2mi ->
       Some (`Sequence None)
    | [x;y;z] when eq x sequence && eq y t2mi ->
       Option.map (fun x -> `Sequence (Some x)) @@ Int.of_string z
    | [a;b;c;d] when eq a section && eq b ts ->
       Option.map2 (fun sid id -> `Section ((Common.Stream.id_of_int32 sid),id))
                   (Int32.of_string c) (Int.of_string d)
    | _ -> None

  open Validation

  let state_query = Filter_one ("state",Bool)

  let get_state_query q     = get state_query q
  let set_state_query v uri = insert state_query v uri

end


module Jitter = struct

  (* TODO IMPLEMENT *)

  include Domain

  let domain = "jitter"

  type req

  let req_to_path : req -> path = function
    | _ -> []
  let req_of_path : path -> req option = function
    | _ -> None

end

type req = [ `Device  of Device.req
           | `Errors  of Errors.req
           | `Streams of Streams.req
           | `Jitter  of Jitter.req
           ]
let req_to_path : req -> Domain.path = function
  | `Device x  -> Device.domain :: Device.req_to_path x
  | `Errors x  -> Errors.domain :: Errors.req_to_path x
  | `Streams x -> Streams.domain :: Streams.req_to_path x
  | `Jitter x  -> Jitter.domain :: Jitter.req_to_path x
let req_of_path : string list -> req option = function
  | hd::tl when eq hd Device.domain  ->
     Option.map (fun x -> `Device x) @@ Device.req_of_path tl
  | hd::tl when eq hd Errors.domain  ->
     Option.map (fun x -> `Errors x) @@ Errors.req_of_path tl
  | hd::tl when eq hd Streams.domain ->
     Option.map (fun x -> `Streams x) @@ Streams.req_of_path tl
  | hd::tl when eq hd Jitter.domain ->
     Option.map (fun x -> `Jitter x) @@ Jitter.req_of_path tl
  | _ -> None
