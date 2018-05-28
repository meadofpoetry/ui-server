open Api.Api_types
open Containers
open Common
open Api.Query

let (^::) = List.cons_maybe

type stream = [ `TS   of Stream.id option
              | `T2MI of int option
              ]

let eq = String.equal

let ts,t2mi = "ts","t2mi"

let stream_to_path : stream -> path = function
  | `TS x   ->
     let sid = Option.map Fun.(Stream.id_to_int32 %> Int32.to_string) x in
     ts :: (sid ^:: [])
  | `T2MI x ->
     let sid = Option.map Int.to_string x in
     ts :: (sid ^:: [])
let stream_of_path : path -> stream option = function
  | [x] when eq x ts   -> Some (`TS None)
  | [x] when eq x t2mi -> Some (`T2MI None)
  | [x;y] when eq x ts ->
     Option.map (fun i -> `TS (Some (Stream.id_of_int32 i))) @@ Int32.of_string y
  | [x;y] when eq x t2mi ->
     Option.map (fun i -> `T2MI (Some i)) @@ Int.of_string y
  | _ -> None

module Domain = struct
  let equal = String.equal
  type path = string list
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
end


module Errors = struct
  include Domain
  let domain  = "errors"
  let segmentation,has_errors = "segmentation","has-errors"

  type req = [ `Errors       of stream
             | `Segmentation of stream
             | `Has_errors   of stream
             ]

  let eq = Domain.equal

  let req_to_path : req -> string list = function
    | `Errors x       -> stream_to_path x
    | `Segmentation x -> segmentation :: stream_to_path x
    | `Has_errors x   -> has_errors :: stream_to_path x
  let req_of_path : string list -> req option = function
    | hd::tl when eq hd ts || eq hd t2mi ->
       Option.map (fun x -> `Errors x) @@ stream_of_path (hd::tl)
    | hd::tl when eq hd segmentation ->
       Option.map (fun x -> `Segmentation x) @@ stream_of_path tl
    | hd::tl when eq hd has_errors ->
       Option.map (fun x -> `Has_errors x) @@ stream_of_path tl
    | _ -> None

end


module Streams = struct
  include Domain
  let domain = "streams"
  let ts,t2mi = "ts","t2mi"
  let input,states,structures,
      bitrates,sequence,section = "input","states","structures",
                                  "bitrates","sequence","section"
  type streams_req_type = [ `All | `Input ]
  type req = [ `Streams    of streams_req_type * Stream.id option
             | `States     of stream
             | `Structures of stream
             | `Bitrates   of Stream.id option
             | `Sequence   of int option
             | `Section    of Stream.id * int
             ]

  let eq = Domain.equal

  let req_to_path : req -> path = function
    | `Streams (t,id) ->
       let sid = Option.map Fun.(Stream.id_to_int32 %> Int32.to_string) id in
       let inp = match t with `All -> None | `Input -> Some input in
       inp ^:: sid ^:: []
    | `States x         -> states :: stream_to_path x
    | `Structures x     -> structures :: stream_to_path x
    | `Bitrates x       -> bitrates :: stream_to_path (`TS x)
    | `Sequence x       -> section :: stream_to_path (`T2MI x)
    | `Section (sid,id) ->
       let ts = stream_to_path (`TS (Some sid)) in
       let id = Int.to_string id in
       (section :: ts) @ (id :: [])
  let req_of_path : path -> req option = function
    | [ ] -> Some (`Streams (`All, None))
    | [x] when eq x input -> Some (`Streams (`Input, None))
    | [x] -> Option.map (fun x -> `Streams (`All, Some (Stream.id_of_int32 x))) @@ Int32.of_string x
    | [x;y] when eq x input ->
       Option.map (fun x -> `Streams (`Input, Some (Stream.id_of_int32 x))) @@ Int32.of_string y
    | hd::tl when eq hd states ->
       Option.map (fun x -> `States x) @@ stream_of_path tl
    | hd::tl when eq hd structures ->
       Option.map (fun x -> `Structures x) @@ stream_of_path tl
    | hd::tl when eq hd bitrates ->
       Option.flat_map (function `TS id -> Some (`Bitrates id) | _ -> None)
       @@ stream_of_path tl
    | [x;y]   when eq x sequence && eq y t2mi ->
       Some (`Sequence None)
    | [x;y;z] when eq x sequence && eq y t2mi ->
       Option.map (fun x -> `Sequence (Some x)) @@ Int.of_string z
    | [a;b;c;d] when eq a section && eq b ts ->
       Option.map2 (fun sid id -> `Section ((Stream.id_of_int32 sid),id))
                   (Int32.of_string c) (Int.of_string d)
    | _ -> None
end

type req = [ `Device  of Device.req
           | `Errors  of Errors.req
           | `Streams of Streams.req
           ]
let req_to_path : req -> Domain.path = function
  | `Device x  -> Device.domain :: Device.req_to_path x
  | `Errors x  -> Errors.domain :: Errors.req_to_path x
  | `Streams x -> Streams.domain :: Streams.req_to_path x
let req_of_path : string list -> req option = function
  | hd::tl when eq hd Device.domain  ->
     Option.map (fun x -> `Device x) @@ Device.req_of_path tl
  | hd::tl when eq hd Errors.domain  ->
     Option.map (fun x -> `Errors x) @@ Errors.req_of_path tl
  | hd::tl when eq hd Streams.domain ->
     Option.map (fun x -> `Streams x) @@ Streams.req_of_path tl
  | _ -> None
