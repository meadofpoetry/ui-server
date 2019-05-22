open Application_types
open Board_niitv_tsan_types
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

  let get_t2mi_mode f control =
    Api_websocket.create
      ~path:Path.Format.("ws/board" @/ Int ^/ "device/mode/t2mi" @/ empty)
      ~query:Query.empty
      control ()
    >>= fun socket ->
    Api_websocket.subscribe_map socket t2mi_mode_of_yojson (f socket);
    Lwt.return_ok socket

end

let reset control =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("api/board" @/ Int ^/ "device/reset" @/ empty)
    ~query:Query.empty
    control
    ignore_env

let set_t2mi_mode mode control =
  Api_http.perform_unit
    ~meth:`POST
    ~body:(t2mi_mode_to_yojson mode)
    ~path:Path.Format.("api/board" @/ Int ^/ "device/mode/t2mi" @/ empty)
    ~query:Query.empty
    control
    ignore_env

let set_input input control =
  Api_http.perform_unit
    ~meth:`POST
    ~body:(input_to_yojson input)
    ~path:Path.Format.("api/board" @/ Int ^/ "mode/input" @/ empty)
    ~query:Query.empty
    control
    ignore_env

let set_port ~port state control =
  Api_http.perform_unit
    ~meth:`POST
    ~body:(Util_json.Bool.to_yojson state)
    ~path:Path.Format.("api/board" @/ Int ^/ "device/mode/port" @/ Int ^/ empty)
    ~query:Query.empty
    control port
    ignore_env

let set_jitter_mode mode control =
  Api_http.perform_unit
    ~meth:`POST
    ~body:(jitter_mode_to_yojson mode)
    ~path:Path.Format.("api/board" @/ Int ^/ "device/mode/jitter" @/ empty)
    ~query:Query.empty
    control
    ignore_env

let get_state control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/state" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Topology.state_of_yojson))

let get_info ?force control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/info" @/ empty)
    ~query:Query.["force", (module Option(Bool))]
    control force
    (ignore_env_bind (Lwt.return % map_err % devinfo_of_yojson))

let get_errors ?force ?timeout control =
  let of_yojson = Util_json.List.of_yojson Deverr.of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/errors" @/ empty)
    ~query:Query.[ "force", (module Option(Bool))
                 ; "timeout", (module Option(Int)) ]
    control force timeout
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_status control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/status" @/empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % status_of_yojson))

let get_input control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/input" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % input_of_yojson))

let get_t2mi_mode ?force control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/mode/t2mi" @/ empty)
    ~query:Query.["force", (module Option(Bool))]
    control force
    (ignore_env_bind (Lwt.return % map_err % t2mi_mode_of_yojson))

let get_jitter_mode ?force control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/mode/jitter" @/ empty)
    ~query:Query.["force", (module Option(Bool))]
    control force
    (ignore_env_bind (Lwt.return % map_err % jitter_mode_of_yojson))
