open Netlib.Uri
open Pipeline_types

module Api_websocket = Api_js.Websocket.Make(Body)

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get_video ?f ?stream ?channel ?pid () =
    let t =
      Api_websocket.create
        ~path:Path.Format.("api/pipeline/measurements/video" @/ empty)
        ~query:Query.[ "stream", (module Option(Application_types.Stream.ID))
                     ; "channel", (module Option(Int))
                     ; "pid", (module Option(Int)) ]
        stream channel pid () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Util_json.List.of_yojson Qoe_errors.Video_data.of_yojson in
      t >>= fun socket ->
      Api_websocket.subscribe_map socket of_json @@ f socket;
      Lwt.return_ok socket

  let get_audio ?f ?stream ?channel ?pid () =
    let t =
      Api_websocket.create
        ~path:Path.Format.("api/pipeline/measurements/audio" @/ empty)
        ~query:Query.[ "stream", (module Option(Application_types.Stream.ID))
                     ; "channel", (module Option(Int))
                     ; "pid", (module Option(Int)) ]
        stream channel pid () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Util_json.List.of_yojson Qoe_errors.Audio_data.of_yojson in
      t >>= fun socket ->
      Api_websocket.subscribe_map socket of_json @@ f socket;
      Lwt.return_ok socket

end
