open Netlib.Uri
open Board_niitv_dvb_types
open Util

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let of_yojson f = Util_json.(
      List.of_yojson @@ Pair.of_yojson Int.of_yojson (ts_of_yojson f))

  let get_measurements ?(ids = []) f control =
    Api_websocket.create
      ~path:Path.Format.("ws/board" @/ Int ^/ "receivers/measurements" @/ empty)
      ~query:Query.["id", (module List(Int))]
      control ids ()
    >>= fun socket ->
    Api_websocket.subscribe_map socket (of_yojson Measure.of_yojson) (f socket);
    Lwt.return_ok socket

  let get_parameters ?(ids = []) f control =
    Api_websocket.create
      ~path:Path.Format.("ws/board" @/ Int ^/ "receivers/parameters" @/ empty)
      ~query:Query.["id", (module List(Int))]
      control ids ()
    >>= fun socket ->
    Api_websocket.subscribe_map socket (of_yojson Params.of_yojson) (f socket);
    Lwt.return_ok socket

  let get_plp_list ?(ids = []) f control =
    Api_websocket.create
      ~path:Path.Format.("ws/board" @/ Int ^/ "receivers/plp-list" @/ empty)
      ~query:Query.["id", (module List(Int))]
      control ids ()
    >>= fun socket ->
    Api_websocket.subscribe_map socket (of_yojson Plp_list.of_yojson) (f socket);
    Lwt.return_ok socket

end

let of_yojson f = Util_json.(Pair.of_yojson Int.of_yojson (ts_of_yojson f))

let set_mode ~id mode control =
  let of_yojson = Util_json.(Pair.of_yojson Int.of_yojson Device.mode_rsp_of_yojson) in
  Api_http.perform
    ~meth:`POST
    ~body:(Device.mode_to_yojson mode)
    ~path:Path.Format.("api/board" @/ Int ^/ "receivers" @/ Int ^/ "mode" @/ empty)
    ~query:Query.empty
    control id
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_stream ~id control =
  let of_yojson = Util_json.Option.of_yojson Application_types.Stream.of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receivers" @/ Int ^/ "stream" @/ empty)
    ~query:Query.empty
    control id
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_measurements ~id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receivers"
                       @/ Int ^/ "measurements" @/ empty)
    ~query:Query.empty
    control id
    (ignore_env_bind (Lwt.return % map_err % of_yojson Measure.of_yojson))

let get_parameters ~id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receivers"
                       @/ Int ^/ "parameters" @/ empty)
    ~query:Query.empty
    control id
    (ignore_env_bind (Lwt.return % map_err % of_yojson Params.of_yojson))

let get_plp_list ~id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "receivers"
                       @/ Int ^/ "plp-list" @/ empty)
    ~query:Query.empty
    control id
    (ignore_env_bind (Lwt.return % map_err % of_yojson Plp_list.of_yojson))
