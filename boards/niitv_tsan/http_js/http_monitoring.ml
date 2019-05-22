open Application_types
open Board_niitv_tsan_types
open Netlib.Uri
open Util

module Event = struct

end

let get_bitrate ?ids ?timeout control =
  let of_yojson = stream_assoc_list_of_yojson Bitrate.of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/bitrate" @/ empty)
    ~query:Query.[ "id", (module Opt_list(Stream.ID))
                 ; "timeout", (module Option(Float)) ]
    control ids timeout
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_ts_info ?ids control =
  let of_yojson = stream_assoc_list_of_yojson TS_info.of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/ts-info" @/ empty)
    ~query:Query.["id", (module Opt_list(Stream.ID))]
    control ids
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_pids ?ids control =
  let of_yojson = stream_assoc_list_of_yojson pids_of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/pids" @/ empty)
    ~query:Query.["id", (module Opt_list(Stream.ID))]
    control ids
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_si_psi_tables ?ids control =
  let of_yojson = stream_assoc_list_of_yojson si_psi_tables_of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/tables" @/ empty)
    ~query:Query.["id", (module Opt_list(Stream.ID))]
    control ids
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_services ?ids control =
  let of_yojson = stream_assoc_list_of_yojson services_of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/services" @/ empty)
    ~query:Query.["id", (module Opt_list(Stream.ID))]
    control ids
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_t2mi_info ?ids control =
  let of_yojson = stream_assoc_list_of_yojson t2mi_info_of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "monitoring/t2mi-info" @/ empty)
    ~query:Query.["id", (module Opt_list(Stream.ID))]
    control ids
    (ignore_env_bind (Lwt.return % map_err % of_yojson))
