open Websocket_cohttp_lwt
open Frame
open Lwt.Infix

module Make (User : Api.USER) (Body : Api.BODY) = struct

  open Netlib

  module Api_http = Api_cohttp.Make (User) (Body)
  
  type node = Api_http.node

  type user = Api_http.user

  type body = Api_http.body

  type state = Api_http.state

  type env = Api.env

  type event = [ `Ev of state * string React.event
               | `Error of string
               ]

  let rand_int () = Random.int 10000000

  let socket_table : (int, unit React.event) Hashtbl.t =
    Hashtbl.create 1000

  let to_response sock_data (event:string React.event) =
    let id = rand_int () in
    (*Cohttp_lwt.Body.drain_body body
    >>= fun () ->*)
    Websocket_cohttp_lwt.upgrade_connection
      (fst sock_data)
      (snd sock_data)
      (fun f -> match f.opcode with
                | Opcode.Close -> Hashtbl.remove socket_table id
                | _ -> ())
    >>= fun (resp, body, frames_out_fn) ->
    let send msg =
      frames_out_fn @@ Some (Frame.create ~content:msg ())
    in
    let sock_events = React.E.map (fun e -> send e) event in
    Hashtbl.add socket_table id sock_events;
    Lwt.return (resp, (body :> Cohttp_lwt.Body.t))

  let transform_resp : event -> Api_http.answer = function
    | `Error _ as e -> e
    | `Ev (state, ev) ->
       `Instant (to_response state ev)

  let transform not_allowed f =
    fun user body env state ->
    if not_allowed user
    then `Error "access denied"
    else transform_resp @@ f user body env state

  let event state ev = `Ev (state, ev)
    
  let node ?doc ?(restrict=[]) ~path ~query handler : node =
    let not_allowed id = List.exists (User.equal id) restrict in
    Uri.Dispatcher.make ?docstring:doc ~path ~query handler
    |> Uri.Dispatcher.map_node (transform not_allowed)
    |> fun node -> `GET, node
    
end

     (*
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
      *)
