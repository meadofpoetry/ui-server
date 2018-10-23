open Containers
open Qoe_errors
open Pipeline_protocol
open Pipeline_api_common
open Api.Interaction
open Api.Interaction.Json
open Common

module WS = struct

  let get_video (api : api) stream channel pid _ body sock_data () =
    match stream, channel, pid with
    | Some s, Some c, Some p ->
       let pred (x : Video_data.t) =
         x.pid = p
         && x.channel = c
         && Stream.ID.equal x.stream s in
       let event = React.E.filter pred api.notifs.vdata in
       Api.Socket.handler socket_table sock_data event
         Video_data.to_yojson body
    | Some s, Some c, _ ->
       let pred (x : Video_data.t) =
         x.channel = c
         && Stream.ID.equal x.stream s in
       let event = React.E.filter pred api.notifs.vdata in
       Api.Socket.handler socket_table sock_data event
         Video_data.to_yojson body
    | Some s, _, _ ->
       let pred (x : Video_data.t) = Stream.ID.equal x.stream s in
       let event = React.E.filter pred api.notifs.vdata in
       Api.Socket.handler socket_table sock_data event
         Video_data.to_yojson body
    | _ ->
       let event = api.notifs.vdata in
       Api.Socket.handler socket_table sock_data event
         Video_data.to_yojson body

end

module HTTP = struct

end

let handler (api : api) =
  let open Uri in
  let open Api_handler in
  create_dispatcher
    "measurements"
    [ create_ws_handler ~docstring:"Video data socket"
        ~path:Path.Format.("video" @/ empty)
        ~query:Query.[ "stream", (module Option(Stream.ID))
                     ; "channel", (module Option(Int))
                     ; "pid", (module Option(Int)) ]
        (WS.get_video api)
    ]
    []
