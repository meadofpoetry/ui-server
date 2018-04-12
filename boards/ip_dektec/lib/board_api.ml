open Containers
open Lwt.Infix
open Api.Interaction
open Board_protocol
open Board_types
open Containers
open Websocket_cohttp_lwt
open Frame

module Api_handler = Api.Handler.Make(Common.User)

let ( % ) = Fun.(%)
   
(* TODO reason about random key *)
let () = Random.init (int_of_float @@ Unix.time ())
let rand_int = fun () -> Random.run (Random.int 10000000)

let socket_table = Hashtbl.create 1000

let set setter _of _to body =
  yojson_of_body body >>= fun x ->
  match _of x with
  | Error e -> respond_error e ()
  | Ok a    ->
     setter a >>= fun a ->
     respond_js (_to a) () 

let address api = set api.addr addr_of_yojson addr_to_yojson

let mask (api : api) = set api.mask mask_of_yojson mask_to_yojson

let gateway (api : api) = set api.gateway gateway_of_yojson gateway_to_yojson

let dhcp (api : api) = set api.dhcp flag_of_yojson flag_to_yojson

let enable (api : api) = set api.enable flag_of_yojson flag_to_yojson

let fec (api : api) = set api.fec flag_of_yojson flag_to_yojson

let port (api : api) = set api.port port_of_yojson port_to_yojson

let meth (api : api) = set api.meth meth_of_yojson meth_to_yojson

let multicast (api : api) = set api.multicast multicast_of_yojson multicast_to_yojson

let delay (api : api) = set api.delay delay_of_yojson delay_to_yojson

let rate_mode (api : api) = set api.rate_mode rate_mode_of_yojson rate_mode_to_yojson

let reset api () =
  api.reset () >>= respond_ok

let config api () =
  api.config () >>= fun conf ->
  respond_js (config_to_yojson conf) ()

let state s_state () =
  respond_js (Common.Topology.state_to_yojson @@ Lwt_react.S.value s_state) ()

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

let devinfo api () =
  api.devinfo () >>= fun info ->
  respond_js (devinfo_to_yojson info) ()

let state_ws sock_data s_state body =
  sock_handler sock_data (Lwt_react.S.changes s_state) Common.Topology.state_to_yojson body

let status_ws sock_data (events : events) body =
  sock_handler sock_data events.status board_status_to_yojson body

let config_ws sock_data (events : events) body =
  sock_handler sock_data events.config config_to_yojson body

let handle api events id s_state _ m args sock_data _ body =
  let open Api.Redirect in
  (* let redirect_if_guest = redirect_if (User.eq id `Guest) in *)
  match m, args with
  | `POST, ["address"]    -> address api body
  | `POST, ["mask"]       -> mask api body
  | `POST, ["gateway"]    -> gateway api body
  | `POST, ["dhcp"]       -> dhcp api body
  | `POST, ["enable"]     -> enable api body
  | `POST, ["fec"]        -> fec api body
  | `POST, ["port"]       -> port api body
  | `POST, ["meth"]       -> meth api body
  | `POST, ["multicast"]  -> multicast api body
  | `POST, ["delay"]      -> delay api body
  | `POST, ["rate_mode"]  -> rate_mode api body
  | `POST, ["reset"]      -> reset api ()
  | `GET,  ["config"]     -> config api ()
  | `GET,  ["state"]      -> state s_state ()
  | `GET,  ["devinfo"]    -> devinfo api ()
  | `GET,  ["status_ws"]  -> status_ws sock_data events body
  | `GET,  ["state_ws"]   -> state_ws sock_data s_state body
  | `GET,  ["config_ws"]  -> config_ws sock_data events body
  | _ -> not_found ()

let handlers id api events s_state =
  [ (module struct
       let domain = Common.Topology.get_api_path id
       let handle = handle api events id s_state
     end : Api_handler.HANDLER) ]
