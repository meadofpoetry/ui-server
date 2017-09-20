open Lwt.Infix
open Interaction
open Board_dvb_protocol
open Common.Board.Dvb
open Containers
open Websocket_cohttp_lwt
open Frame

(* TODO reason about random key *)
let () = Random.init (int_of_float @@ Unix.time ())
let rand_int = fun () -> Random.run (Random.int 10000000)

let socket_table = Hashtbl.create 1000
   
let devinfo api =
  api.devinfo () >>= fun devi ->
  respond_js (devinfo_response_to_yojson devi) ()
   
let reset api =
  api.reset () >>= respond_ok

let settings (api : api) body =
  yojson_of_body body >>= fun set ->
  match settings_request_of_yojson set with
  | Error e -> respond_error e ()
  | Ok set  ->
     api.settings set >>= fun set_rsp ->
     respond_js (settings_response_to_yojson set_rsp) ()

let plp_setting api body =
  yojson_of_body body >>= fun plp ->
  match plp_setting_request_of_yojson plp with
  | Error e    -> respond_error e ()
  | Ok plp_set ->
     api.plp_setting plp_set >>= fun plp_set_rsp ->
     respond_js (plp_setting_response_to_yojson plp_set_rsp) ()

let plps (api : api) num =
  try
    let i = int_of_string num in
    api.plps i >>= fun plp_rsp ->
    respond_js (plp_list_response_to_yojson plp_rsp) ()
  with _ -> respond_error (Printf.sprintf "plps: bad argument %s" num) ()

let measures sock_data events body =
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
    let msg = Msg_conv.to_string @@ x in
    frames_out_fn @@ Some (Frame.create ~content:msg ())
  in
  let sock_events = Lwt_react.E.map (send % measure_to_yojson) events.measure in
  Hashtbl.add socket_table id sock_events;
  Lwt.return (resp, (body :> Cohttp_lwt_body.t))
          
let handle api events _ meth args sock_data _ body =
  let open Lwt.Infix in
  let open Redirect in
  (* let redirect_if_guest = redirect_if (User.eq id `Guest) in *)
  match meth, args with
  | `GET,  ["devinfo"]      -> devinfo api
  | `POST, ["reset"]        -> reset api
  | `POST, ["settings"]     -> settings api body
  | `POST, ["plp_setting"]  -> plp_setting api body
  | `GET,  "plps"::[num]    -> plps api num
  | _,     ["measures"]     -> measures sock_data events body
  | _ -> not_found ()

let handlers id api events =
  [ (module struct
       let domain = Common.Hardware.get_api_path id
       let handle = handle api events
     end : Api_handler.HANDLER) ]
