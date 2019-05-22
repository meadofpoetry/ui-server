open Application_types
open Board_niitv_tsan_types
open Netlib.Uri
open Util

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get_streams ?incoming ?(ids = []) f control =
    let of_yojson = Util_json.List.of_yojson Stream.of_yojson in
    Api_websocket.create
      ~path:Path.Format.("ws/board" @/ Int ^/ "streams" @/ empty)
      ~query:Query.[ "incoming", (module Option(Bool))
                   ; "id", (module List(Stream.ID))]
      control incoming ids ()
    >>= fun socket ->
    Api_websocket.subscribe_map socket of_yojson (f socket);
    Lwt.return_ok socket


end

let get_streams ?(ids = []) ?incoming control =
  let of_yojson = Util_json.List.of_yojson Stream.of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams" @/ empty)
    ~query:Query.[ "id", (module List(Stream.ID))
                 ; "incoming", (module Option(Bool)) ]
    control ids incoming
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_stream id control =
  let of_yojson = Util_json.Option.of_yojson Stream.of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams" @/ Stream.ID.fmt ^/ empty)
    ~query:Query.empty
    control id
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_bitrate id control =
  let of_yojson = Util_json.Option.of_yojson Bitrate.of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams"
                       @/ Stream.ID.fmt ^/ "bitrate" @/ empty)
    ~query:Query.empty
    control id
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_ts_info ?force id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams"
                       @/ Stream.ID.fmt ^/ "ts-info" @/ empty)
    ~query:Query.["force", (module Option(Bool))]
    control id force
    (ignore_env_bind (Lwt.return % map_err % TS_info.of_yojson))

let get_pids ?force id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams"
                       @/ Stream.ID.fmt ^/ "pids" @/ empty)
    ~query:Query.["force", (module Option(Bool))]
    control id force
    (ignore_env_bind (Lwt.return % map_err % pids_of_yojson))

let get_si_psi_tables ?force id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams"
                       @/ Stream.ID.fmt ^/ "tables" @/ empty)
    ~query:Query.["force", (module Option(Bool))]
    control id force
    (ignore_env_bind (Lwt.return % map_err % si_psi_tables_of_yojson))

let get_services ?force id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams"
                       @/ Stream.ID.fmt ^/ "services" @/ empty)
    ~query:Query.["force", (module Option(Bool))]
    control id force
    (ignore_env_bind (Lwt.return % map_err % services_of_yojson))

let get_t2mi_info ?force id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams"
                       @/ Stream.ID.fmt ^/ "t2mi-info" @/ empty)
    ~query:Query.["force", (module Option(Bool))]
    control id force
    (ignore_env_bind (Lwt.return % map_err % t2mi_info_of_yojson))

let get_t2mi_sequence ?duration ?(t2mi_stream_id = []) id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams"
                       @/ Stream.ID.fmt ^/ "t2mi-sequence" @/ empty)
    ~query:Query.[ "duration", (module Option(Int))
                 ; "t2mi-stream-id", (module List(Int)) ]
    control id duration t2mi_stream_id
    (ignore_env_bind (Lwt.return % map_err % ts_of_yojson T2mi_sequence.of_yojson))

let get_section ?section ?table_id_ext ?id_ext_1 ?id_ext_2 ~table id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams"
                       @/ Stream.ID.fmt ^/ "section" @/ Int ^/ empty)
    ~query:Query.[ "section", (module Option(Int))
                 ; "table-id-ext", (module Option(Int))
                 ; "id-ext-1", (module Option(Int))
                 ; "id-ext-2", (module Option(Int)) ]
    control id table
    section table_id_ext id_ext_1 id_ext_2
    (ignore_env_bind (Lwt.return % map_err % ts_of_yojson SI_PSI_section.Dump.of_yojson))
