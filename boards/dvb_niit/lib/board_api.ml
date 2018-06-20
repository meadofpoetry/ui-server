open Api.Interaction
open Board_protocol
open Board_types
open Containers
open Websocket_cohttp_lwt
open Frame

let ( >>= ) = Json.( >>= )
let ( >|= ) = Lwt.Infix.( >|= )
let ( %> )  = Fun.( %> )

module Api_handler = Api.Handler.Make(Common.User)

(* TODO reason about random key *)
let () = Random.init (int_of_float @@ Unix.time ())
let rand_int = fun () -> Random.run (Random.int 10000000)

let socket_table = Hashtbl.create 1000

let devinfo api =
  api.get_devinfo () >|= (devinfo_opt_to_yojson %> Result.return)
  >>= Json.respond_result

let config api =
  api.get_config () >|= (config_to_yojson %> Result.return)
  >>= Json.respond_result

let reset api =
  api.reset () >|= Result.return
  >>= Json.respond_result_unit

let state s_state =
  Lwt_react.S.value s_state
  |> (Common.Topology.state_to_yojson %> Result.return)
  |> Json.respond_result

let settings (api : api) body =
  Json.of_body body >>= fun set ->
  (match mode_req_of_yojson set with
   | Error e -> Lwt_result.fail @@ Json.of_error_string e
   | Ok set  -> api.set_mode set >|= (mode_rsp_to_yojson %> Result.return))
  >>= Json.respond_result

let plp_setting api body =
  Json.of_body body >>= fun plp ->
  (match plp_set_req_of_yojson plp with
   | Error e -> Lwt_result.fail @@ Json.of_error_string e
   | Ok set  -> api.set_plp set >|= (plp_set_rsp_to_yojson %> Result.return))
  >>= Json.respond_result

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
  sock_handler sock_data (React.S.changes s_state) Common.Topology.state_to_yojson body

let config_ws sock_data (events : events) body =
  sock_handler sock_data events.config config_to_yojson body

let measures_ws sock_data (events : events) body =
  sock_handler sock_data events.measures measures_to_yojson body

let handle api (events:events) id _ meth uri_sep sock_data headers body =
  let open Lwt.Infix in
  let open Api.Redirect in
  (* let redirect_if_guest = redirect_if (User.eq id `Guest) in *)
  (* TODO match string + query *)
  let path_list = Common.Uri.(split @@  Path.to_string uri_sep.path) in
  match Api.Headers.is_ws headers, meth, path_list with
  | _,`GET,  ["devinfo"]     -> devinfo api
  | _,`GET,  ["config"]      -> config api
  | false,`GET,  ["device";"state"]       -> state events.state

  | _,`POST, ["reset"]       -> reset api
  | _,`POST, ["settings"]    -> settings api body
  | _,`POST, ["plp_setting"] -> plp_setting api body

  | true,`GET,["device";"state"] -> state_ws sock_data events.state body
  | _,`GET,  ["config_ws"]   -> config_ws sock_data events body
  | _,`GET,  ["measures_ws"] -> measures_ws sock_data events body
  | _ -> not_found ()

let handlers id api events =
  [ (module struct
       let domain = Common.Topology.get_api_path id
       let handle = handle api events id
     end : Api_handler.HANDLER) ]
