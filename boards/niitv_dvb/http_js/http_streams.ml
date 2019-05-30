open Netlib.Uri
open Board_niitv_dvb_types
open Application_types
open Util

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let of_yojson f =
    Util_json.(List.of_yojson @@ Pair.of_yojson Stream.ID.of_yojson (ts_of_yojson f))

  let get_streams ?(ids = []) sock control =
    let of_yojson = Util_json.List.of_yojson Stream.of_yojson in
    Api_websocket.subscribe
      ~path:Path.Format.("board" @/ Int ^/ "streams" @/ empty)
      ~query:Query.["id", (module List(Stream.ID))]
      control ids of_yojson sock

  let get_measurements ?(ids = []) sock control =
    Api_websocket.subscribe
      ~path:Path.Format.("board" @/ Int ^/ "streams/measurements" @/ empty)
      ~query:Query.["id", (module List(Stream.ID))]
      control ids (of_yojson Measure.of_yojson) sock

  let get_parameters ?(ids = []) sock control =
    Api_websocket.subscribe
      ~path:Path.Format.("board" @/ Int ^/ "streams/parameters" @/ empty)
      ~query:Query.["id", (module List(Stream.ID))]
      control ids (of_yojson Params.of_yojson) sock

end

let of_yojson f = Util_json.(Pair.of_yojson Int.of_yojson (ts_of_yojson f))

let get_measurements (id : Stream.ID.t) control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams"
                       @/ Stream.ID.fmt ^/ "measurements" @/ empty)
    ~query:Query.empty
    control id
    (ignore_env_bind (Lwt.return % map_err % of_yojson Measure.of_yojson))

let get_parameters (id : Stream.ID.t) control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams"
                       @/ Stream.ID.fmt ^/ "parameters" @/ empty)
    ~query:Query.empty
    control id
    (ignore_env_bind (Lwt.return % map_err % of_yojson Params.of_yojson))

let get_stream (id : Stream.ID.t) control =
  let of_yojson = Util_json.Option.of_yojson Stream.of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams" @/ Stream.ID.fmt ^/ empty)
    ~query:Query.empty
    control id
    (ignore_env_bind (Lwt.return % map_err % of_yojson))

let get_streams ?(ids = []) control =
  let of_yojson = Util_json.List.of_yojson Stream.of_yojson in
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/board" @/ Int ^/ "streams" @/ empty)
    ~query:Query.["id", (module List(Stream.ID))]
    control ids
    (ignore_env_bind (Lwt.return % map_err % of_yojson))
