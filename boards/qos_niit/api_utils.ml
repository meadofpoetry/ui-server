open Containers
open Api_common

type stream = [ `TS   of Common.Stream.id option
              | `T2MI of int option
              ]

type err = Bad_query of Common.Uri.Query.err
         | Other     of string [@@deriving yojson]

let eq = String.equal

let ts,t2mi = "ts","t2mi"
let stream_to_path : stream -> string list = function
  | `TS x   -> let sid = Option.map Fun.(Common.Stream.id_to_int32 %> Int32.to_string) x in
               ts :: (sid ^:: [])
  | `T2MI x -> let sid = Option.map Int.to_string x in
               ts :: (sid ^:: [])
let stream_of_path : string list -> stream option = function
  | [x]   when eq x ts   -> Some (`TS None)
  | [x]   when eq x t2mi -> Some (`T2MI None)
  | [x;y] when eq x ts   -> Option.map (fun i -> `TS (Some (Common.Stream.id_of_int32 i)))
                            @@ Int32.of_string y
  | [x;y] when eq x t2mi -> Option.map (fun i -> `T2MI (Some i)) @@ Int.of_string y
  | _                    -> None

module Device = struct
  include (Domain: module type of Domain with module Query := Domain.Query)

  let domain = "device"
  let errors,mode,info,
      port,state,status,
      reset = "errors","mode","info",
              "port","state","status",
              "reset"

  type mode = [ `T2MI | `JITTER ]
  type req  = [ Device.req
              | `Info
              | `Errors
              | `Mode of mode
              | `Status
              | `Reset
              ]

  let eq = String.equal
  let mode_to_string = function `T2MI -> "t2mi" | `JITTER -> "jitter"
  let mode_of_string = function
    | x when eq x "t2mi"   -> Some `T2MI
    | x when eq x "jitter" -> Some `JITTER
    | _                    -> None

  let req_to_path : req -> path = function
    | `Info   -> [info]
    | `Errors -> [errors]
    | `Mode x -> [mode;mode_to_string x]
    | `Status -> [status]
    | `Reset  -> [reset]
    | (`Port _ | `State) as x -> Device.req_to_path x
  let req_of_path path : req option =
    match Device.req_of_path path with
    | Some _ as x -> (x :> req option)
    | None        -> (match path with
                      | [x] when eq x info   -> Some `Info
                      | [x] when eq x errors -> Some `Errors
                      | [x] when eq x status -> Some `Status
                      | [x;y] when eq x mode -> Option.map (fun x -> `Mode x) @@ mode_of_string y
                      | _                    -> None)

  module Query = struct
    include Device.Query
    let errors_query = Filter ("errors",Int)
  end

end


module Errors = struct
  include (Domain: module type of Domain with module Query := Domain.Query)
  let domain  = "errors"
  let percent,has_any = "percent","has-any"

  type req = [ `Errors  of stream
             | `Percent of stream
             | `Has_any of stream
             ]

  let eq = String.equal

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

  module Query = struct
    include Domain.Query
    let errors_query = Filter ("errors",Int)
    let level_query  = Filter ("level",Int)
  end

end

module Streams = struct
  include (Domain: module type of Domain with module Query := Domain.Query)
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

  let eq = String.equal

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

  module Query = struct
    include Domain.Query
    let state_query          = Filter_one ("state",Bool)
    let section_query        = One ("section",Int)
    let table_id_ext_query   = One ("table-id-ext",Int)
    let eit_ts_id_query      = One ("eit-ts-id",Int)
    let eit_orig_nw_id_query = One ("eit-orig-nw-id",Int)
    let seconds_query        = One ("seconds",Int)

  end

end


module Jitter = struct
  include (Domain: module type of Domain with module Query := Domain.Query)

  (* TODO IMPLEMENT *)

  let domain = "jitter"

  type req

  let req_to_path : req -> path = function
    | _ -> []
  let req_of_path : path -> req option = function
    | _ -> None

  module Query = struct
    include Domain.Query
  end

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
