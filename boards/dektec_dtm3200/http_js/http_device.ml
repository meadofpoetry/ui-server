open Application_types
open Board_dektec_dtm3200_types
open Netlib.Uri
open Util

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get_state sock control =
    Api_websocket.subscribe
      ~path:Path.Format.("board" @/ Int ^/ "device/state" @/ empty)
      ~query:Query.empty
      control Topology.state_of_yojson sock

  let get_info sock control =
    Api_websocket.subscribe
      ~path:Path.Format.("board" @/ Int ^/ "device/info" @/ empty)
      ~query:Query.empty
      control devinfo_of_yojson sock

  let get_config sock control =
    Api_websocket.subscribe
      ~path:Path.Format.("ws/board" @/ Int ^/ "device/config" @/ empty)
      ~query:Query.empty
      control config_of_yojson sock

end

let get_state control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/state" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Topology.state_of_yojson))

let get_info control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/info" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % devinfo_of_yojson))

let get_config control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/config" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % config_of_yojson))

let get_fpga_version control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/fpga-version" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_hardware_version control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/hardware-version" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_firmware_version control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/firmware-version" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_serial_number control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/serial-number" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))

let get_type control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/type" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Util_json.Int.of_yojson))
