open Board_niitv_dvb_types
open Netlib.Uri
open Api_common

module Event = struct

  let ( % ) f g x = f (g x)

  let ( >>= ) = Lwt_result.( >>= )

  let map_ok f = function
    | Ok x -> Ok (f x)
    | Error e -> Error e

  let get_state ?f control =
    let t =
      Api_websocket.create
        ~path:Path.Format.(get_api_path control @/ "device/state" @/ empty)
        ~query:Query.empty () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Application_types.Topology.state_of_yojson in
      t >>= fun socket ->
      Api_websocket.subscribe (f % map_ok of_json) socket;
      Lwt.return_ok socket

  let get_receivers ?on_error ?f control =
    let t =
      Api_websocket.create ?on_error
        ~path:Path.Format.(get_api_path control @/ "device/receivers" @/ empty)
        ~query:Query.empty () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Util_json.(Option.of_yojson @@ List.of_yojson Int.of_yojson) in
      t >>= fun socket ->
      Api_websocket.subscribe (f % map_ok of_json) socket;
      Lwt.return_ok socket

  let get_mode ?(ids = []) ?on_error ?f control =
    let t =
      Api_websocket.create ?on_error
        ~path:Path.Format.(get_api_path control @/ "device/mode" @/ empty)
        ~query:Query.["id", (module List(Int))]
        ids () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Device.config_of_yojson in
      t >>= fun socket ->
      Api_websocket.subscribe (f % map_ok of_json) socket;
      Lwt.return_ok socket

end

let reset control =
  Api_http.perform
    ~meth:`POST
    ~path:Path.Format.(get_api_path control @/ "device/reset" @/ empty)
    ~query:Query.empty
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Device.info_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let set_mode ~id mode control =
  Api_http.perform
    ~meth:`POST
    ~body:(Device.mode_to_yojson mode)
    ~path:Path.Format.(get_api_path control @/ "device/mode" @/ Int ^/ empty)
    ~query:Query.empty
    id
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Util_json.(Pair.of_yojson Int.of_yojson Device.mode_rsp_of_yojson) x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_state control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(get_api_path control @/ "device/state" @/ empty)
    ~query:Query.empty
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Application_types.Topology.state_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_info control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(get_api_path control @/ "device/info" @/ empty)
    ~query:Query.empty
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Util_json.Option.of_yojson Device.info_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_receivers control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(get_api_path control @/ "device/receivers" @/ empty)
    ~query:Query.empty
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Util_json.(Option.of_yojson @@ List.of_yojson Int.of_yojson) x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)

let get_mode ?(ids = []) control =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.(get_api_path control @/ "device/mode" @/ empty)
    ~query:Query.["id", (module List(Int))]
    ids
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Device.config_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)
