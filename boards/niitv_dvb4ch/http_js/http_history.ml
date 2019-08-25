open Netlib.Uri
open Board_niitv_dvb4ch_types
open Util

let of_yojson f =
  Api.rows_of_yojson
    Util_json.(List.of_yojson @@ Pair.of_yojson Int.of_yojson (List.of_yojson (ts_of_yojson f)))
    (fun _ -> Error "expected rows")

let get_measurements ?(ids = []) ?limit ?from ?till ?duration control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "measurements" @/ empty)
    ~query:Query.[ "id", (module List(Int))
                 ; "limit", (module Option(Int))
                 ; "from", (module Option(Time_uri.Show))
                 ; "to", (module Option(Time_uri.Show))
                 ; "duration", (module Option(Time_uri.Show_relative)) ]
    control ids limit from till duration
    (ignore_env_bind (Lwt.return % map_err % of_yojson Measure.of_yojson))
