open Lwt.Infix
open Api.Interaction
open Board_protocol
open Board_types
open Containers
open Websocket_cohttp_lwt
open Frame

module Api_handler = Api.Handler.Make(Common.User)

let ( % ) = CCFun.(%)
   
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

let multicast (api : api) = set api.multicast multicast_of_yojson multicast_to_yojson

let delay (api : api) = set api.delay delay_of_yojson delay_to_yojson

let rate_mode (api : api) = set api.rate_mode rate_mode_of_yojson rate_mode_to_yojson

let reset api () =
  api.reset () >>= respond_ok

let config api () =
  api.config () >>= fun conf ->
  respond_js (config_to_yojson conf) ()

let status sock_data (events : events) body =
  let id = rand_int () in
  Cohttp_lwt_body.drain_body body
  >>= fun () ->
  Websocket_cohttp_lwt.upgrade_connection
    (fst sock_data)
    (snd sock_data)
    (fun f -> match f.opcode with
              | Opcode.Close -> Hashtbl.remove socket_table id
              | _ -> ())
  >>= fun (resp, body, frames_out_fn) ->
  let send x =
    let msg = Api.Msg_conv.to_string @@ x in
    frames_out_fn @@ Some (Frame.create ~content:msg ())
  in
  let sock_events = Lwt_react.E.map (send % board_status_to_yojson) events.status in
  Hashtbl.add socket_table id sock_events;
  Lwt.return (resp, (body :> Cohttp_lwt_body.t))

let handle api events _ meth args sock_data _ body =
  let open Api.Redirect in
  (* let redirect_if_guest = redirect_if (User.eq id `Guest) in *)
  match meth, args with
  | `POST, ["address"]   -> address api body
  | `POST, ["mask"]      -> mask api body
  | `POST, ["gateway"]   -> gateway api body
  | `POST, ["dhcp"]      -> dhcp api body
  | `POST, ["enable"]    -> enable api body
  | `POST, ["fec"]       -> fec api body
  | `POST, ["port"]      -> port api body
  | `POST, ["multicast"] -> multicast api body
  | `POST, ["delay"]     -> delay api body
  | `POST, ["rate_mode"] -> rate_mode api body
  | `POST, ["reset"]     -> reset api ()
  | `GET,  ["config"]    -> config api ()
  | `GET,  ["status"]    -> status sock_data events body
  | _ -> not_found ()

let handlers id api events =
  [ (module struct
       let domain = Common.Hardware.get_api_path id
       let handle = handle api events
     end : Api_handler.HANDLER) ]
