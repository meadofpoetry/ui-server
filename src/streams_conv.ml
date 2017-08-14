type stream =
  { stream   : int
  ; uri      : string 
  ; channels : Common.Streams.channel list
  } [@@deriving yojson, lens { optional = true } ]

type t =
  { prog_list         : stream list option [@default None]
  } [@@deriving yojson, lens { optional = true } ]

let to_streams conv (s : t) : Common.Streams.t =
  let open CCOpt in 
  let open Common.Streams in
  { prog_list = s.prog_list >|= (fun s ->
      List.map (fun x -> { input    = conv x.stream
                         ; uri      = x.uri
                         ; channels = x.channels } )
               s )
  }

let of_streams conv (s : Common.Streams.t) : t =
  let open CCOpt in 
  let open Common.Streams in
  { prog_list = s.prog_list >|= (fun s ->
      List.map (fun x -> { stream   = conv x.input
                         ; uri      = x.uri
                         ; channels = x.channels } )
               s )
  }

let streams_of_yojson conv js =
  let open CCResult in
  of_yojson js
  >|= (to_streams conv)

let streams_to_yojson conv s =
  of_streams conv s
  |> to_yojson

let dump_streams s =
  let open CCOpt in
  let open Common.Streams in
  s.prog_list
  >|= List.map (fun prog -> (prog.input, Msg_conv.to_string @@ stream_to_yojson prog))
