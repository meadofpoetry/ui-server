open Board_niitv_dvb_types
open Netlib.Uri

module Api_http = Api_js.Http.Make(Application_types.Body)

let get_api_path = Application_types.Topology.get_api_path

(* module WS = struct
 * 
 *   open Netlib.Uri
 * 
 *   include Boards_js.Requests.Device.WS
 * 
 *   let get_receivers control =
 *     WS.get ~from:Json.(Option.of_yojson @@ List.of_yojson Int.of_yojson)
 *       ~path:Path.Format.(get_base_path () / ("receivers" @/ empty))
 *       ~query:Query.empty
 *       control
 * 
 *   let get_mode ?(ids = []) control =
 *     WS.get ~from:config_of_yojson
 *       ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
 *       ~query:Query.["id", (module List(Int))]
 *       control ids
 * 
 * end *)

module Event = struct

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
