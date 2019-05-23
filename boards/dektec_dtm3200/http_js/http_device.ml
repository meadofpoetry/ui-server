open Application_types
open Board_dektec_dtm3200_types
open Netlib.Uri
open Util

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get_state f control =
    Api_websocket.create
      ~path:Path.Format.("ws/board" @/ Int ^/ "device/state" @/ empty)
      ~query:Query.empty
      control ()
    >>= fun socket ->
    Api_websocket.subscribe_map socket Topology.state_of_yojson (f socket);
    Lwt.return_ok socket

  let get_info f control =
    Api_websocket.create
      ~path:Path.Format.("ws/board" @/ Int ^/ "device/info" @/ empty)
      ~query:Query.empty
      control ()
    >>= fun socket ->
    Api_websocket.subscribe_map socket devinfo_of_yojson (f socket);
    Lwt.return_ok socket

  let get_config f control =
    Api_websocket.create
      ~path:Path.Format.("ws/board" @/ Int ^/ "device/config" @/ empty)
      ~query:Query.empty
      control ()
    >>= fun socket ->
    Api_websocket.subscribe_map socket config_of_yojson (f socket);
    Lwt.return_ok socket

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
