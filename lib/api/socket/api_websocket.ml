open Websocket_cohttp_lwt
open Websocket
open Lwt.Infix

module type CONTROL_MSG = sig
  type t
  val parse : t -> (int * string) option
  val compose : int -> t -> t
end

module Json_msg : CONTROL_MSG with type t = Yojson.Safe.json = struct
  type t = Yojson.Safe.json

  let parse = function
    | `Assoc [ "id", `Int id
             ; "path", `String path
      ] -> Some (id, path)
    | _ -> None

  let compose id data =
    `Assoc [ "id", `Int id
           ; "data", data
      ]
end
   
module Make
         (User : Api.USER)
         (Body : Api.BODY)
         (Msg : CONTROL_MSG with type t = Body.t) = struct

  open Netlib

  module Api_http = Api_cohttp.Make (User) (Body)

  type node = Api_http.node

  type user = Api_http.user

  type event_node = (user -> Body.t React.event Lwt.t) Uri.Dispatcher.node
                  
  type t = (user -> Body.t React.event Lwt.t) Uri.Dispatcher.t

  let transform not_allowed f =
    fun user ->
    if not_allowed user
    then Lwt.fail_with "not allowed"
    else f user
         
  let event_node ?doc ?(restrict = []) ~path ~query event_source : event_node =
    let not_allowed id = List.exists (User.equal id) restrict in
    Uri.Dispatcher.make ?docstring:doc ~path ~query event_source
    |> Uri.Dispatcher.map_node (transform not_allowed)

  let make ?prefix nodes =
     let add_node map node =
      let node = match prefix with
        | None -> node
        | Some prefix ->
           assert (String.length prefix <> 0);
           Uri.Dispatcher.prepend (Uri.Path.of_string prefix) node
      in
      Uri.Dispatcher.(add map node)
    in 
    List.fold_left add_node Uri.Dispatcher.empty nodes

  let merge ?prefix handlers =
    match prefix with
    | None ->
       Uri.Dispatcher.concat handlers
    | Some prefix ->
       assert (String.length prefix <> 0);
       let path = Uri.Path.of_string prefix in
       Uri.Dispatcher.(merge empty [path, handlers])

  let connections = Array.make 1_000 None

  let find_free_connection () =
    let rec loop ind =
      if ind >= Array.length connections
      then failwith "too much socket connections"
      else
        match connections.(ind) with
        | None -> ind
        | Some _ -> loop (succ ind)
    in loop 0

  let store_connection ind data =
    connections.(ind) <- Some data

  let free_connection ind =
    connections.(ind) <- None
                  
  let conn_mutex = Lwt_mutex.create ()
                  
  let open_connection (events : t) user _body _env state =

    Lwt_mutex.with_lock conn_mutex
      (fun () ->
        
        let connected_events = Hashtbl.create 100 in

        let msg_stream, push_msg = React.E.create () in

        let conn_id = find_free_connection () in
    
        let add id path =
          try
            begin
              match Hashtbl.find_opt connected_events id with
              | None -> ()
              | Some _ -> failwith "event exists"
            end;
            Netlib.Uri.Dispatcher.dispatch
              ~default:(fun _ -> raise Not_found)
              events
              (Uri.of_string path)
              user
            >>= fun event ->
            let event' = React.E.map (fun ev -> push_msg @@ Msg.compose id ev) event in
            Hashtbl.add connected_events id event';
            Lwt.return_unit
          with _ -> Lwt.return_unit (* TODO log or respond *)
        
        in
        
        let parse_frame s =
          match Body.of_string s with
          | Error _ -> None
          | Ok v -> Msg.parse v
        in
        
        Websocket_cohttp_lwt.upgrade_connection
          (fst state)
          (fun frame ->
            match frame.opcode with
            | Frame.Opcode.Close ->
               free_connection conn_id
            | _ ->
               match parse_frame frame.content with
               | None -> ()
               | Some (id, path) -> Lwt.async (fun () -> add id path))
        >>= fun (resp, frames_out_fn) ->

        let send msg =
          let msg = Body.to_string msg in 
          frames_out_fn @@ Some (Frame.create ~content:msg ())
        in

        let event_stream = React.E.map send msg_stream in
        store_connection conn_id (event_stream, connected_events);
    
        Lwt.return resp)

  let to_http ?doc ~prefix nodes =
    let make_conn user body env (state : Api_http.state) : Api_http.answer Lwt.t =
      Lwt.return (`Instant (open_connection nodes user body env state))
    in
    Api_http.make
      [ Api_http.node_raw
          ?doc
          ~meth:`GET
          ~path:Uri.Path.Format.(prefix @/ empty)
          ~query:Uri.Query.empty
          make_conn
      ]

end
