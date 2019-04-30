open Netlib.Uri
open Application_types
open Api_common
open Pipeline_types

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get_video ?on_error ?f ?stream ?channel ?pid () =
    let t =
      Api_websocket.create ?on_error
        ~path:Path.Format.("api/pipeline/measurements/video" @/ empty)
        ~query:Query.[ "stream", (module Option(Stream.ID))
                     ; "channel", (module Option(Int))
                     ; "pid", (module Option(Int)) ]
        stream channel pid () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Util_json.List.of_yojson Qoe_errors.Video_data.of_yojson in
      t >>= fun socket ->
      Api_websocket.subscribe (f % map_ok of_json) socket;
      Lwt.return_ok socket

  let get_audio ?on_error ?f ?stream ?channel ?pid () =
    let t =
      Api_websocket.create ?on_error
        ~path:Path.Format.("api/pipeline/measurements/audio" @/ empty)
        ~query:Query.[ "stream", (module Option(Stream.ID))
                     ; "channel", (module Option(Int))
                     ; "pid", (module Option(Int)) ]
        stream channel pid () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Util_json.List.of_yojson Qoe_errors.Audio_data.of_yojson in
      t >>= fun socket ->
      Api_websocket.subscribe (f % map_ok of_json) socket;
      Lwt.return_ok socket

end
