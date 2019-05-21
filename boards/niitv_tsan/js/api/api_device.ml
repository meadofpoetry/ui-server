open Application_types
open Board_niitv_tsan_types
open Netlib.Uri
open Api_common

module Event = struct

end

let reset control =
  Api_http.perform_unit
    ~path:Path.Format.("api/board" @/ Int ^/ "device/reset" @/ empty)
    ~query:Query.empty
    control
    ignore_env

let set_t2mi_mode mode control =
  Api_http.perform_unit
    ~body:(t2mi_mode_to_yojson mode)
    ~path:Path.Format.("api/board" @/ Int ^/ "device/mode/t2mi" @/ empty)
    ~query:Query.empty
    control
    ignore_env

let set_input input control =
  Api_http.perform_unit
    ~body:(input_to_yojson input)
    ~path:Path.Format.("api/board" @/ Int ^/ "mode/input" @/ empty)
    ~query:Query.empty
    control
    ignore_env

let set_port ~port state control =
  Api_http.perform_unit
    ~body:(Util_json.Bool.to_yojson state)
    ~path:Path.Format.("api/board" @/ Int ^/ "device/mode/port" @/ Int ^/ empty)
    ~query:Query.empty
    control port
    ignore_env

let set_jitter_mode mode control =
  Api_http.perform_unit
    ~body:(jitter_mode_to_yojson mode)
    ~path:Path.Format.("api/board" @/ Int ^/ "device/mode/jitter" @/ empty)
    ~query:Query.empty
    control
    ignore_env

let get_state control =
  Api_http.perform
    ~path:Path.Format.("api/board" @/ Int ^/ "device/state" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % Topology.state_of_yojson))

let get_info control =
  Api_http.perform
    ~path:Path.Format.("api/board" @/ Int ^/ "device/info" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % devinfo_of_yojson))
