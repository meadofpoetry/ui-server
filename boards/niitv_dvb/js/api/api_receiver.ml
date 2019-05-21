open Netlib.Uri
open Board_niitv_dvb_types
open Api_common

module Event = struct

  let ( % ) f g x = f (g x)

  let ( >>= ) = Lwt_result.( >>= )

  let map_ok f = function Error e -> Error e | Ok x -> Ok (f x)

  let of_json f = Util_json.(
      List.of_yojson @@ Pair.of_yojson Int.of_yojson (ts_of_yojson f))

  let get_measurements ?(ids = []) ?f control =
    let t =
      Api_websocket.create
        ~path:Path.Format.(string_of_int control
                           @/ "receiver/measurements"
                           @/ empty)
        ~query:Query.["id", (module List(Int))]
        ids () in
    match f with
    | None -> t
    | Some f ->
      t >>= fun socket ->
      Api_websocket.subscribe socket (f % map_ok (of_json Measure.of_yojson));
      Lwt.return_ok socket

  let get_parameters ?(ids = []) ?on_error ?f control =
    let t =
      Api_websocket.create ?on_error
        ~path:Path.Format.(get_api_path control @/ "receiver/parameters" @/ empty)
        ~query:Query.["id", (module List(Int))]
        ids () in
    match f with
    | None -> t
    | Some f ->
      t >>= fun socket ->
      Api_websocket.subscribe (f % map_ok (of_json Params.of_yojson)) socket;
      Lwt.return_ok socket

  let get_plp_list ?(ids = []) ?on_error ?f control =
    let t =
      Api_websocket.create ?on_error
        ~path:Path.Format.(get_api_path control @/ "receiver/plp-list" @/ empty)
        ~query:Query.["id", (module List(Int))]
        ids () in
    match f with
    | None -> t
    | Some f ->
      t >>= fun socket ->
      Api_websocket.subscribe (f % map_ok (of_json Plp_list.of_yojson)) socket;
      Lwt.return_ok socket

end

let of_json f = Util_json.(Pair.of_yojson Int.of_yojson (ts_of_yojson f))

let get_stream ~id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(string_of_int control
                       @/ "receiver"
                       @/ Int
                       ^/ "stream"
                       @/ empty)
    ~query:Query.empty
    id (fun _env -> function
        | Error e -> Lwt.return_error e
        | Ok x ->
          match Util_json.Option.of_yojson Application_types.Stream.of_yojson x with
          | Error e -> Lwt.return_error (`Conv_error e)
          | Ok x -> Lwt.return_ok x)

let get_measurements ~id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(get_api_path control
                       @/ "receiver"
                       @/ Int
                       ^/ "measurements" @/ empty)
    ~query:Query.empty
    id (fun _env -> function
        | Error e -> Lwt.return_error e
        | Ok x ->
          match of_json Measure.of_yojson x with
          | Error e -> Lwt.return_error (`Conv_error e)
          | Ok x -> Lwt.return_ok x)

let get_parameters ~id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(get_api_path control
                       @/ "receiver"
                       @/ Int
                       ^/ "parameters" @/ empty)
    ~query:Query.empty
    id (fun _env -> function
        | Error e -> Lwt.return_error e
        | Ok x ->
          match of_json Params.of_yojson x with
          | Error e -> Lwt.return_error (`Conv_error e)
          | Ok x -> Lwt.return_ok x)

let get_plp_list ~id control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(get_api_path control
                       @/ "receiver"
                       @/ Int
                       ^/ "plp-list" @/ empty)
    ~query:Query.empty
    id (fun _env -> function
        | Error e -> Lwt.return_error e
        | Ok x ->
          match of_json Plp_list.of_yojson x with
          | Error e -> Lwt.return_error (`Conv_error e)
          | Ok x -> Lwt.return_ok x)
