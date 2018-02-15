open Containers
open Lwt.Infix
open Api.Interaction
open Board_protocol
open Board_types
open Websocket_cohttp_lwt
open Frame

module Api_handler = Api.Handler.Make(Common.User)

let ( % ) = Fun.(%)

(* TODO reason about random key *)
let () = Random.init (int_of_float @@ Unix.time ())
let rand_int = fun () -> Random.run (Random.int 10000000)

let socket_table = Hashtbl.create 1000

let devinfo (api:api) () =
  api.devinfo () >>= fun devi ->
  respond_js (devinfo_response_to_yojson devi) ()

let state s_state =
  respond_js (Common.Topology.state_to_yojson @@ React.S.value s_state) ()

let config (api:api) ()=
  api.config () >>= fun cfg ->
  respond_js (config_response_to_yojson cfg) ()

let streams s_streams =
  respond_js (Common.Stream.t_list_to_yojson @@ React.S.value s_streams) ()

let set_factory_mode (api:api) body () =
  yojson_of_body body >>= fun mode ->
  match factory_settings_of_yojson mode with
  | Error e -> respond_error e ()
  | Ok mode -> api.set_factory_mode mode >>= respond_ok

let set_nw_mode (api:api) body () =
  yojson_of_body body >>= fun mode ->
  match nw_settings_of_yojson mode with
  | Error e -> respond_error e ()
  | Ok mode -> api.set_mode mode >>= respond_ok

let set_streams_simple (api:api) body () =
  yojson_of_body body >>= fun sms ->
  match Common.Stream.t_list_of_yojson sms with
  | Error e -> respond_error e ()
  | Ok sms  -> api.set_streams_simple sms
               >>= function
               | Error e -> respond_error e ()
               | Ok ()   -> respond_ok ()

let set_streams_full (api:api) body () =
  yojson_of_body body >>= fun sms ->
  match streams_full_request_of_yojson sms with
  | Error e -> respond_error e ()
  | Ok sms  -> api.set_streams_full sms
               >>= function
               | Error e -> respond_error e ()
               | Ok ()   -> respond_ok ()

let sock_handler sock_data (event:'a React.event) (to_yojson:'a -> Yojson.Safe.json) body =
  let id = rand_int () in
  Cohttp_lwt.Body.drain_body body
  >>= fun () ->
  Websocket_cohttp_lwt.upgrade_connection
    (fst sock_data)
    (snd sock_data)
    (fun f -> match f.opcode with
              | Opcode.Close -> Hashtbl.remove socket_table id
              | _ -> ())
  >>= fun (resp, body, frames_out_fn) ->
  let send x =
    let msg = Yojson.Safe.to_string x in
    frames_out_fn @@ Some (Frame.create ~content:msg ())
  in
  let sock_events = Lwt_react.E.map (send % to_yojson) event in
  Hashtbl.add socket_table id sock_events;
  Lwt.return (resp, (body :> Cohttp_lwt.Body.t))

let state_ws sock_data s_state body =
  sock_handler sock_data (Lwt_react.S.changes s_state) Common.Topology.state_to_yojson body

let status_ws sock_data (events : events) body =
  sock_handler sock_data events.status status_to_yojson body

let config_ws sock_data (events : events) body =
  sock_handler sock_data events.config config_response_to_yojson body

let streams_ws sock_data s_streams body =
  sock_handler sock_data (Lwt_react.S.changes s_streams) Common.Stream.t_list_to_yojson body

let handle api events s_state s_streams _ meth args sock_data _ body =
  let open Api.Redirect in
  match meth, args with
  | `POST, ["factory_mode"]   -> set_factory_mode api body ()
  | `POST, ["nw_mode"]        -> set_nw_mode api body ()
  | `POST, ["streams_simple"] -> set_streams_simple api body ()
  | `POST, ["streams_full"]   -> set_streams_full api body ()

  | `GET,  ["devinfo"]        -> devinfo api ()
  | `GET,  ["state"]          -> state s_state
  | `GET,  ["config"]         -> config api ()
  | `GET,  ["streams"]        -> streams s_streams

  | `GET,  ["state_ws"]       -> state_ws sock_data s_state body
  | `GET,  ["status_ws"]      -> status_ws sock_data events body
  | `GET,  ["config_ws"]      -> config_ws sock_data events body
  | `GET,  ["streams_ws"]     -> streams_ws sock_data s_streams body
  | _ -> not_found ()

let handlers id api events s_state s_streams =
  [ (module struct
       let domain = Common.Topology.get_api_path id
       let handle = handle api events s_state s_streams
     end : Api_handler.HANDLER) ]
