open Websocket_cohttp_lwt
open Websocket
open Lwt.Infix

module Make (User : Api.USER) (Body : Api.BODY) = struct

  open Netlib

  module Api_http = Api_cohttp.Make (User) (Body)
  
  type node = Api_http.node

  type user = Api_http.user

  type body = Api_http.body

  type state = Api_http.state

  type env = Api.env

  type event = [ `Ev of body React.event
               | `Error of string
               ]

  type socket_table = (int, unit React.event * (unit -> unit)) Hashtbl.t

  let make_socket_table () =
    Hashtbl.create 1000

  let close_sockets (table : socket_table) =
    Hashtbl.iter (fun _ (_,close) -> close ()) table;
    Hashtbl.clear table

  let rand_int () = Random.int 10000000
    
  (* TODO fix api *)
  let to_response socket_table sock_data (event:body React.event) =
    let id = rand_int () in
    (*Cohttp_lwt.Body.drain_body body
    >>= fun () ->*)
    Websocket_cohttp_lwt.upgrade_connection
      (fst sock_data)
      (fun f -> match f.opcode with
                | Frame.Opcode.Close ->
                   Hashtbl.find_all socket_table id
                   |> List.iter (fun (_,close) -> close ());
                   Hashtbl.remove socket_table id
                | _ -> ())
    >>= fun (resp, frames_out_fn) ->
    
    let send msg =
      let msg = Body.to_string msg in 
      frames_out_fn @@ Some (Frame.create ~content:msg ())
    in
    
    let sock_events = React.E.map (fun e -> send e) event in
    
    let close () =
      React.E.stop sock_events;
      frames_out_fn @@ Some (Frame.create ~opcode:Frame.Opcode.Close ())
    in
    
    Hashtbl.add socket_table id (sock_events, close);
    Lwt.return resp

  let transform_resp socket_table state : event -> Api_http.answer = function
    | `Error _ as e -> e
    | `Ev ev ->
       `Instant (to_response socket_table state ev)

  let transform socket_table not_allowed f =
    fun user body env state ->
    if not_allowed user
    then Lwt.return (`Error "access denied")
    else Lwt.map
           (transform_resp socket_table state)
           (f user body env state)

  let event ev : event Lwt.t = Lwt.return (`Ev ev)
    
  let node ?doc ?(restrict=[]) ~socket_table ~path ~query handler : node =
    let not_allowed id = List.exists (User.equal id) restrict in
    Uri.Dispatcher.make ?docstring:doc ~path ~query handler
    |> Uri.Dispatcher.map_node (transform socket_table not_allowed)
    |> fun node -> `GET, node
    
end
