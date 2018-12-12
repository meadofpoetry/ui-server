open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path () =
  Uri.Path.Format.(Boards_js.Requests.get_board_path ()
                   / ("transmitter" @/ empty))

module WS = struct

  open Common.Uri

  let get_status control =
    WS.get ~from:status_of_yojson
      ~path:Path.Format.(get_base_path () / ("status" @/ empty))
      ~query:Query.empty
      control

  let get_mode control =
    WS.get ~from:(Json.List.of_yojson packer_settings_of_yojson)
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.empty
      control

  let get_in_streams control =
    WS.get ~from:(Json.List.of_yojson Stream.of_yojson)
      ~path:Path.Format.(get_base_path () / ("streams/input" @/ empty))
      ~query:Query.empty
      control

  let get_out_streams control =
    WS.get ~from:(Json.List.of_yojson Stream.of_yojson)
      ~path:Path.Format.(get_base_path () / ("streams/output" @/ empty))
      ~query:Query.empty
      control

end

module HTTP = struct

  open Common.Uri

  let set_mode (mode:stream_settings list) control =
    post_result_unit ~contents:((Json.List.to_yojson stream_settings_to_yojson) mode)
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.empty
      control

  let set_streams (streams:Stream.t list) control =
    post_result_unit ~contents:((Json.List.to_yojson Stream.to_yojson) streams)
      ~path:Path.Format.(get_base_path () / ("streams" @/ empty))
      ~query:Query.empty
      control

  let get_mode control =
    get_result ~from:(Json.List.of_yojson packer_settings_of_yojson)
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.empty
      control

  let get_in_streams control =
    get_result ~from:(Json.List.of_yojson Stream.of_yojson)
      ~path:Path.Format.(get_base_path () / ("streams/input" @/ empty))
      ~query:Query.empty
      control

  let get_out_streams control =
    get_result ~from:(Json.List.of_yojson Stream.of_yojson)
      ~path:Path.Format.(get_base_path () / ("streams/output" @/ empty))
      ~query:Query.empty
      control

  let get_status control =
    get_result ~from:(Json.Option.of_yojson status_of_yojson)
      ~path:Path.Format.(get_base_path () / ("status" @/ empty))
      ~query:Query.empty
      control

end
