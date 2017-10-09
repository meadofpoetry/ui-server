type stream =
  { stream   : int
  ; uri      : string
  ; channels : Streams.channel list
  } [@@deriving yojson, lens { optional = true } ]

type t = stream list [@@deriving yojson]

let to_streams conv (s : t) : Streams.t =
  let open Streams in
  List.map (fun x -> { input    = conv x.stream
                     ; id       = Int32.of_int x.stream
                     ; uri      = x.uri
                     ; channels = x.channels } )
           s

let of_streams conv (s : Streams.t) : t =
  let open Streams in
  List.map (fun x -> { stream   = conv x.input
                     ; uri      = x.uri
                     ; channels = x.channels } )
           s

let streams_of_yojson conv js =
  let open CCResult in
  of_yojson js
  >|= (to_streams conv)

let streams_to_yojson conv s =
  of_streams conv s
  |> to_yojson

let dump_streams s =
  let open Streams in
  List.map (fun prog -> (prog.input, Api.Msg_conv.to_string @@ stream_to_yojson prog)) s
