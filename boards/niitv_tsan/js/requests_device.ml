open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path = Boards_js.Requests.Device.get_device_path

module WS = struct

  open Common.Uri

  include Boards_js.Requests.Device.WS

  let get_status control =
    let path = Path.Format.(get_base_path () / ("status" @/ empty)) in
    WS.get ~from:status_of_yojson ~path ~query:Query.empty control

  let get_errors ?(errors = []) control =
    WS.get ~from:(Json.List.of_yojson Board_error.of_yojson)
      ~path:Path.Format.(get_base_path () / ("errors" @/ empty))
      ~query:Query.["errors", (module List(Int))]
      control errors

  let get_t2mi_mode control =
    WS.get ~from:(Json.Option.of_yojson t2mi_mode_of_yojson)
      ~path:Path.Format.(get_base_path () / ("mode/t2mi" @/ empty))
      ~query:Query.empty
      control

  let get_jitter_mode control =
    WS.get ~from:(Json.Option.of_yojson jitter_mode_of_yojson)
      ~path:Path.Format.(get_base_path () / ("mode/jitter" @/ empty))
      ~query:Query.empty
      control

end

module HTTP = struct

  open Common.Uri

  include (Boards_js.Requests.Device.HTTP:
           module type of Boards_js.Requests.Device.HTTP
                          with module Archive := Boards_js.Requests.Device.HTTP.Archive)

  let post_port ~port ~state control =
    post_result ~from:input_of_yojson
      ~path:Uri.Path.Format.(get_base_path () / ("port" @/ Int ^/ Bool ^/ empty))
      ~query:Uri.Query.empty
      control port state

  let post_reset control =
    post_result_unit ~path:Path.Format.(get_base_path () / ("reset" @/ empty))
      ~query:Query.empty
      control

  let post_t2mi_mode mode control =
    post_result ~from:(Json.Option.of_yojson t2mi_mode_of_yojson)
      ~contents:((Json.Option.to_yojson t2mi_mode_to_yojson) mode)
      ~path:Path.Format.(get_base_path () / ("mode/t2mi" @/ empty))
      ~query:Query.empty
      control

  let post_jitter_mode mode control =
    post_result ~from:(Json.Option.of_yojson jitter_mode_of_yojson)
      ~contents:((Json.Option.to_yojson jitter_mode_to_yojson) mode)
      ~path:Path.Format.(get_base_path () / ("mode/jitter" @/ empty))
      ~query:Query.empty
      control

  let get_devinfo control =
    get_result ~from:(Json.Option.of_yojson devinfo_of_yojson)
      ~path:Path.Format.(get_base_path () / ("info" @/ empty))
      ~query:Query.empty
      control

  let get_t2mi_mode control =
    get_result ~from:(Json.Option.of_yojson t2mi_mode_of_yojson)
      ~path:Path.Format.(get_base_path () / ("mode/t2mi" @/ empty))
      ~query:Query.empty
      control

  let get_jitter_mode control =
    get_result ~from:(Json.Option.of_yojson jitter_mode_of_yojson)
      ~path:Path.Format.(get_base_path () / ("mode/jitter" @/ empty))
      ~query:Query.empty
      control

  module Archive = struct

    let of_yojson =
      Api_js.Api_types.rows_of_yojson
        (Json.List.of_yojson state_of_yojson)
        state_compressed_of_yojson

    let get_state ?limit ?compress ?from ?till ?duration control =
      get_result ~from:of_yojson
        ~path:Uri.Path.Format.(get_base_path () / ("state/archive" @/ empty))
        ~query:Uri.Query.[ "limit",    (module Option(Int))
                         ; "compress", (module Option(Bool))
                         ; "from",     (module Option(Time.Show))
                         ; "to",       (module Option(Time.Show))
                         ; "duration", (module Option(Time.Relative))]
        control limit compress from till duration

    let get_errors ?(errors=[]) ?limit ?compress ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / ("errors/archive" @/ empty))
        ~query:Query.[ "errors",   (module List(Int))
                     ; "limit",    (module Option(Int))
                     ; "compress", (module Option(Bool))
                     ; "from",     (module Option(Time.Show))
                     ; "to",       (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control errors limit compress from till duration

  end

end
