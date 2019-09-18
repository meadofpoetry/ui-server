open Application_types
open Board_niitv_tsan_types

let map_err = function
  | Error e -> Error (`Msg e)
  | Ok _ as x -> x

let ignore_env _ x = Lwt.return x

let ignore_env_bind f _ = function
  | Error _ as e -> Lwt.return e
  | Ok x -> f x

module Api_http = Api_js.Http.Make (Application_types.Body)

let stream_assoc_list_of_yojson _of =
  Util_json.(List.of_yojson (Pair.of_yojson Stream.ID.of_yojson _of))

let pids_of_yojson =
  Util_json.(List.of_yojson @@ Pair.of_yojson Int.of_yojson PID.of_yojson)

let pids_ts_of_yojson = ts_of_yojson pids_of_yojson

let si_psi_tables_of_yojson =
  Util_json.(
    List.of_yojson @@ Pair.of_yojson SI_PSI_table.id_of_yojson SI_PSI_table.of_yojson)

let si_psi_tables_ts_of_yojson = ts_of_yojson si_psi_tables_of_yojson

let services_of_yojson =
  Util_json.(List.of_yojson @@ Pair.of_yojson Int.of_yojson Service.of_yojson)

let services_ts_of_yojson = ts_of_yojson services_of_yojson

let t2mi_info_of_yojson =
  Util_json.(List.of_yojson @@ Pair.of_yojson Int.of_yojson T2mi_info.of_yojson)
