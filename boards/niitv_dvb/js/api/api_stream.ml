open Netlib.Uri
open Board_niitv_dvb_types
open Application_types
open Api_common

module Event = struct

  let ( % ) f g x = f (g x)

  let ( >>= ) = Lwt_result.( >>= )

  let map_ok f = function Error e -> Error e | Ok x -> Ok (f x)

  let of_json f =
    Util_json.(List.of_yojson @@ Pair.of_yojson Stream.ID.of_yojson (ts_of_yojson f))

  let get_measurements ?(ids = []) ?on_error ?f control =
    let t =
      Api_websocket.create ?on_error
        ~path:Path.Format.(get_api_path control @/ "stream/measurements" @/ empty)
        ~query:Query.["id", (module List(Stream.ID))]
        ids () in
    match f with
    | None -> t
    | Some f ->
      t >>= fun socket ->
      Api_websocket.subscribe (f % map_ok (of_json Measure.of_yojson)) socket;
      Lwt.return_ok socket

  let get_parameters ?(ids = []) ?on_error ?f control =
    let t =
      Api_websocket.create ?on_error
        ~path:Path.Format.(get_api_path control @/ "stream/parameters" @/ empty)
        ~query:Query.["id", (module List(Stream.ID))]
        ids () in
    match f with
    | None -> t
    | Some f ->
      t >>= fun socket ->
      Api_websocket.subscribe (f % map_ok (of_json Params.of_yojson)) socket;
      Lwt.return_ok socket

  let get_streams ?(ids = []) ?on_error ?f control =
    let t =
      Api_websocket.create ?on_error
        ~path:Path.Format.(get_api_path control @/ "stream" @/ empty)
        ~query:Query.["id", (module List(Stream.ID))]
        ids () in
    match f with
    | None -> t
    | Some f ->
      t >>= fun socket ->
      let of_json = Util_json.List.of_yojson Stream.of_yojson in
      Api_websocket.subscribe (f % map_ok of_json) socket;
      Lwt.return_ok socket

end

let of_json f = Util_json.(Pair.of_yojson Int.of_yojson (ts_of_yojson f))

let get_measurements (id : Stream.ID.t) control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(get_api_path control
                       @/ "stream"
                       @/ Stream.ID.fmt
                       ^/ "measurements" @/ empty)
    ~query:Query.empty
    id (fun _env -> function
        | Error e -> Lwt.return_error e
        | Ok x ->
          match of_json Measure.of_yojson x with
          | Error e -> Lwt.return_error (`Conv_error e)
          | Ok x -> Lwt.return_ok x)

let get_parameters (id : Stream.ID.t) control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(get_api_path control
                       @/ "stream"
                       @/ Stream.ID.fmt
                       ^/ "parameters" @/ empty)
    ~query:Query.empty
    id (fun _env -> function
        | Error e -> Lwt.return_error e
        | Ok x ->
          match of_json Params.of_yojson x with
          | Error e -> Lwt.return_error (`Conv_error e)
          | Ok x -> Lwt.return_ok x)

let get_stream (id : Stream.ID.t) control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(get_api_path control
                       @/ "stream"
                       @/ Stream.ID.fmt ^/ empty)
    ~query:Query.empty
    id (fun _env -> function
        | Error e -> Lwt.return_error e
        | Ok x ->
          match Util_json.Option.of_yojson Stream.of_yojson x with
          | Error e -> Lwt.return_error (`Conv_error e)
          | Ok x -> Lwt.return_ok x)

let get_streams ?(ids = []) control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(get_api_path control @/ "stream" @/ empty)
    ~query:Query.["id", (module List(Stream.ID))]
    ids (fun _env -> function
        | Error e -> Lwt.return_error e
        | Ok x ->
          match Util_json.List.of_yojson Stream.of_yojson x with
          | Error e -> Lwt.return_error (`Conv_error e)
          | Ok x -> Lwt.return_ok x)
