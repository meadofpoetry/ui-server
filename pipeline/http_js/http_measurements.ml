open Netlib.Uri
open Pipeline_types

module Api_websocket = Api_js.Websocket.Make(Body)(Api_js.Websocket.Json_msg)

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get_video ?stream ?channel ?pid sock =
    let of_yojson = Util_json.List.of_yojson Qoe_errors.Video_data.of_yojson in
    Api_websocket.subscribe
      ~path:Path.Format.("ws/pipeline/measurements/video" @/ empty)
      ~query:Query.[ "stream", (module Option(Application_types.Stream.ID))
                   ; "channel", (module Option(Int))
                   ; "pid", (module Option(Int)) ]
      stream channel pid of_yojson sock

  let get_audio ?stream ?channel ?pid sock =
    let of_yojson = Util_json.List.of_yojson Qoe_errors.Audio_data.of_yojson in
    Api_websocket.subscribe
      ~path:Path.Format.("ws/pipeline/measurements/audio" @/ empty)
      ~query:Query.[ "stream", (module Option(Application_types.Stream.ID))
                   ; "channel", (module Option(Int))
                   ; "pid", (module Option(Int)) ]
      stream channel pid of_yojson sock

end
