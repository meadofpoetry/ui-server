open Application_types
open Board_niitv_tsan_types
open Netlib.Uri
open Util

module Event = struct
  let get_bitrate ?(ids = []) sock control =
    let of_yojson = stream_assoc_list_of_yojson Bitrate.of_yojson in
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("board" @/ Int ^/ "monitoring/bitrate" @/ empty)
      ~query:Query.["id", (module List (Stream.ID))]
      control
      ids
      of_yojson
      sock

  let get_pids ?(ids = []) sock control =
    let of_yojson = stream_assoc_list_of_yojson pids_ts_of_yojson in
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("board" @/ Int ^/ "monitoring/pids" @/ empty)
      ~query:Query.["id", (module List (Stream.ID))]
      control
      ids
      of_yojson
      sock

  let get_services ?(ids = []) sock control =
    let of_yojson = stream_assoc_list_of_yojson services_ts_of_yojson in
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("board" @/ Int ^/ "monitoring/services" @/ empty)
      ~query:Query.["id", (module List (Stream.ID))]
      control
      ids
      of_yojson
      sock
end

let get_errors ?(ids = []) ?timeout ?(pids = []) ?(priority = []) control =
  let of_yojson =
    stream_assoc_list_of_yojson @@ Util_json.List.of_yojson Error.of_yojson
  in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/errors" @/ empty)
    ~query:
      Query.
        [ "id", (module List (Stream.ID))
        ; "timeout", (module Option (Int))
        ; "pid", (module List (Int))
        ; "priority", (module List (Int)) ]
    control
    ids
    timeout
    pids
    priority
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_bitrate ?(ids = []) ?timeout control =
  let of_yojson = stream_assoc_list_of_yojson Bitrate.of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/bitrate" @/ empty)
    ~query:Query.["id", (module List (Stream.ID)); "timeout", (module Option (Int))]
    control
    ids
    timeout
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_ts_info ?force ?(ids = []) control =
  let of_yojson = stream_assoc_list_of_yojson TS_info.of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/ts-info" @/ empty)
    ~query:Query.["id", (module List (Stream.ID)); "force", (module Option (Bool))]
    control
    ids
    force
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_pids ?force ?(ids = []) control =
  let of_yojson = stream_assoc_list_of_yojson pids_ts_of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/pids" @/ empty)
    ~query:Query.["id", (module List (Stream.ID)); "force", (module Option (Bool))]
    control
    ids
    force
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_si_psi_tables ?force ?(ids = []) control =
  let of_yojson = stream_assoc_list_of_yojson si_psi_tables_of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/tables" @/ empty)
    ~query:Query.["id", (module List (Stream.ID)); "force", (module Option (Bool))]
    control
    ids
    force
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_services ?force ?(ids = []) control =
  let of_yojson = stream_assoc_list_of_yojson services_ts_of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/services" @/ empty)
    ~query:Query.["id", (module List (Stream.ID)); "force", (module Option (Bool))]
    control
    ids
    force
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_t2mi_info ?force ?(ids = []) ?(t2mi_stream_ids = []) control =
  let of_yojson = stream_assoc_list_of_yojson t2mi_info_of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/t2mi-info" @/ empty)
    ~query:
      Query.
        [ "id", (module List (Stream.ID))
        ; "t2mi-stream-id", (module List (Int))
        ; "force", (module Option (Bool)) ]
    control
    ids
    t2mi_stream_ids
    force
    (ignore_env_bind (Lwt.return % map_err % of_yojson))
