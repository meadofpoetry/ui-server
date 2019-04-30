open Netlib.Uri
open Api_common
open Board_niitv_dvb_types

type meas = (int * Measure.t ts list) list

let of_json f =
  Api.rows_of_yojson
    Util_json.(List.of_yojson @@ Pair.of_yojson Int.of_yojson (List.of_yojson (ts_of_yojson f)))
    (fun _ -> Error "expected rows")

let get_measurements ?(ids = []) ?limit ?from ?till ?duration control =
  Api_http.perform
    ~path:Path.Format.(get_api_path control @/ "measurements" @/ empty)
    ~query:Query.[ "id", (module List(Int))
                 ; "limit", (module Option(Int))
                 ; "from", (module Option(Time_uri.Show))
                 ; "to", (module Option(Time_uri.Show))
                 ; "duration", (module Option(Time_uri.Show_relative)) ]
    ids limit from till duration
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match of_json Measure.of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)
