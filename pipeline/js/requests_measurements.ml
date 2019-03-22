open Api_js.Requests.Json_request
open Common

module WS = struct

  open Common.Uri

  let get_video ?stream ?channel ?pid () =
    WS.get ~path:Path.Format.("api/pipeline/measurements/video" @/ empty)
      ~query:Query.[ "stream", (module Option(Stream.ID))
                   ; "channel", (module Option(Int))
                   ; "pid", (module Option(Int)) ]
      ~from:(Json.List.of_yojson Qoe_errors.Video_data.of_yojson)
      stream channel pid

  let get_audio ?stream ?channel ?pid () =
    WS.get ~path:Path.Format.("api/pipeline/measurements/audio" @/ empty)
      ~query:Query.[ "stream", (module Option(Stream.ID))
                   ; "channel", (module Option(Int))
                   ; "pid", (module Option(Int)) ]
      ~from:(Json.List.of_yojson Qoe_errors.Audio_data.of_yojson)
      stream channel pid

end
