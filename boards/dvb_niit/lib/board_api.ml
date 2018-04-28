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
  api.devinfo () >|= (devinfo_response_to_yojson %> Result.return)
  >>= Json.respond_result

let config api =
  api.config () >|= (config_to_yojson %> Result.return)
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
  (match settings_request_of_yojson set with
   | Error e -> Lwt_result.fail @@ Json.of_error_string e
   | Ok set  -> api.settings set >|= (settings_response_to_yojson %> Result.return))
  >>= Json.respond_result

let plp_setting api body =
  Json.of_body body >>= fun plp ->
  (match plp_setting_request_of_yojson plp with
   | Error e -> Lwt_result.fail @@ Json.of_error_string e
   | Ok set  -> api.plp_setting set >|= (plp_setting_response_to_yojson %> Result.return))
  >>= Json.respond_result

let plps (api : api) num =
  (match Int.of_string num with
   | Some i -> api.plps i >|= (plp_list_response_to_yojson %> Result.return)
   | None   -> Lwt_result.fail @@ Json.of_error_string (Printf.sprintf "bad argument: %s" num))
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
  sock_handler sock_data events.measure measure_response_to_yojson body

let handle api events id s_state _ meth args sock_data _ body =
  let open Lwt.Infix in
  let open Api.Redirect in
  (* let redirect_if_guest = redirect_if (User.eq id `Guest) in *)
  match meth, args with
  | `GET,  ["devinfo"]     -> devinfo api
  | `GET,  "plps"::[num]   -> plps api num
  | `GET,  ["config"]      -> config api
  | `GET,  ["state"]       -> state s_state

  | `POST, ["reset"]       -> reset api
  | `POST, ["settings"]    -> settings api body
  | `POST, ["plp_setting"] -> plp_setting api body

  | `GET,  ["state_ws"]    -> state_ws sock_data s_state body
  | `GET,  ["config_ws"]   -> config_ws sock_data events body
  | `GET,  ["measures_ws"] -> measures_ws sock_data events body
  | _ -> not_found ()

let handlers id api events s_state =
  [ (module struct
       let domain = Common.Topology.get_api_path id
       let handle = handle api events id s_state
     end : Api_handler.HANDLER) ]
