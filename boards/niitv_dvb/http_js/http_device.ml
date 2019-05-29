open Application_types
open Board_niitv_dvb_types.Device
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

  let get_mode ?(ids = []) f control =
    let of_yojson = Util_json.(
        List.of_yojson (Pair.of_yojson Int.of_yojson mode_of_yojson)) in
    Api_websocket.create
      ~path:Path.Format.("ws/board" @/ Int ^/ "device/mode" @/ empty)
      ~query:Query.["id", (module List(Int))]
      control ids ()
    >>= fun socket ->
    Api_websocket.subscribe_map socket of_yojson (f socket);
    Lwt.return_ok socket

end

let reset control =
  Api_http.perform
    ~meth:`POST
    ~path:Path.Format.("api/board" @/ Int ^/ "device/reset" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % info_of_yojson))

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
    (ignore_env_bind (Lwt.return % map_err % info_of_yojson))

let get_receivers control =
  let of_yojson = Util_json.(Option.of_yojson @@ List.of_yojson Int.of_yojson) in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/receivers" @/ empty)
    ~query:Query.empty
    control
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_mode ?(ids = []) control =
  let of_yojson = Util_json.(
    List.of_yojson (Pair.of_yojson Int.of_yojson mode_of_yojson)) in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "device/mode" @/ empty)
    ~query:Query.["id", (module List(Int))]
    control ids
    (ignore_env_bind (Lwt.return % map_err % of_yojson))
