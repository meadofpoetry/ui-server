open Containers
open Api.Interaction
open Api.Redirect
open Common.Topology
open Boards.Board

open Websocket_cohttp_lwt
open Frame
open Lwt.Infix
open Api.Interaction
   
open Application
open Application_types

(* TODO reason about random key *)
let () = Random.init (int_of_float @@ Unix.time ())
let rand_int = fun () -> Random.run (Random.int 10000000)

let socket_table = Hashtbl.create 1000

let get_page () =
  respond_html_elt
    Tyxml.Html.(div
                  [ h2 [ pcdata "Hardware page" ];
                    p  [ pcdata "Some text" ];
                    div ~a:[ a_id "app_container" ] [  ] ] )
    ()

let get_stream_table app () =
  Json.respond_result @@ Ok (stream_table_to_yojson (React.S.value app.hw.streams))

let get_stream_table_socket sock_data body app () =
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
    let msg = Yojson.Safe.to_string @@ stream_table_to_yojson x in
    frames_out_fn @@ Some (Frame.create ~content:msg ())
  in
  let sock_events = Lwt_react.S.map send app.hw.streams in
  Hashtbl.add socket_table id sock_events;
  Lwt.return (resp, (body :> Cohttp_lwt.Body.t))

let set_stream_settings body app () =
  Json.of_body body >>= fun js ->
  match stream_setting_of_yojson js with
  | Error e -> respond_error e ()
  | Ok s    ->
     Hardware.set_stream app.hw s >>= function
     | Ok () as r -> Json.respond_result_unit r
     | Error ejs  -> Json.respond_result_unit (Error (set_error_to_yojson ejs))

let get_topology app () =
  Json.respond_result @@ Ok (to_yojson (React.S.value app.topo))
  
let get_topology_socket sock_data body app () =
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
    let msg = Yojson.Safe.to_string @@ to_yojson x in
    frames_out_fn @@ Some (Frame.create ~content:msg ())
  in
  let sock_events = Lwt_react.S.map send app.topo in
  Hashtbl.add socket_table id sock_events;
  Lwt.return (resp, (body :> Cohttp_lwt.Body.t))

  
  
let handle app id meth uri sock_data _ body =
  let open Common.Uri in
  let is_guest = Common.User.eq id `Guest in
  match Scheme.is_ws uri.scheme, meth, uri.path with
  | _,    `GET, []                    -> get_page ()
  | true, `GET, ["topology"]          -> get_topology_socket sock_data body app ()
  | _,    `GET, ["topology"]          -> get_topology app ()
  | true, `GET, ["stream_table"]      -> get_stream_table_socket sock_data body app ()
  | _,    `GET, ["stream_table"]      -> get_stream_table app ()
  | _,    `POST, ["stream_settings"]  -> redirect_if is_guest @@ set_stream_settings body app
  | _        -> Api.Redirect.not_found ()

let handlers app =
  let hls = Hardware.Map.fold (fun _ x acc -> x.handlers @ acc) app.hw.boards [] in
  let proc_api = match app.proc with
    | None      -> []
    | Some proc -> proc#handlers ()
  in
  [ Api_handler.add_layer "board" hls ;
    (module struct
       let domain = "topology"
       let handle = handle app
     end : Api_handler.HANDLER) ]
  @ proc_api
  @ (Pc_control.Network_api.handlers app.network)
  @ (User_api.handlers app.users)
