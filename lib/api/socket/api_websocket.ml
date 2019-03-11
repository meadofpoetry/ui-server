open Websocket_cohttp_lwt
open Frame
open Api_cohttp.Interaction.Json

type id = ..
   
(* TODO reason about random key *)
let rand_int () = Random.int 10000000
                
let socket_table : (id * int, unit React.event) Hashtbl.t =
  Hashtbl.create 1000
                
let handler id sock_data (event:'a React.event) (to_yojson:'a -> Yojson.Safe.json) body =
  let id = id, rand_int () in
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
  let sock_events = React.E.map (fun e -> send @@ to_yojson e) event in
  Hashtbl.add socket_table id sock_events;
  Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
