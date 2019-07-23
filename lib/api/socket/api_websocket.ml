open Websocket
open Lwt.Infix

module Make
    (User : Api.USER)
    (Body : Api.BODY)
    (Msg : Api.WS_BODY with type t = Body.t) = struct

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

  let open_connection (events : t) (ping : unit React.event) user _body _env state =

    Lwt_mutex.with_lock conn_mutex
      (fun () ->

         let subscriptions = Hashtbl.create 100 in

         let msg_stream, push_msg = React.E.create () in

         let conn_id = find_free_connection () in

         let unsubscribe (subid : int) =
           match Hashtbl.find_opt subscriptions subid with
           | None -> Error "No such subscription"
           | Some (n, e) ->
             match !n with
             | 0 -> React.E.stop e; Hashtbl.remove subscriptions subid; Ok ()
             | _ -> decr n; Ok () in

         let subscribe (uri : string) =
           let subid = Hashtbl.hash uri in
           match Hashtbl.find_opt subscriptions subid with
           | Some (n, _) -> incr n; Lwt.return_ok subid
           | None ->
             try
               Netlib.Uri.Dispatcher.dispatch
                 ~default:(fun _ -> raise Not_found)
                 events
                 (Uri.of_string uri)
                 user
               >>= fun event ->
               let event' = React.E.map (fun ev ->
                   let msg = Msg.compose subid (`Event ev) in
                   push_msg (`Body msg)) event in
               Hashtbl.add subscriptions subid (ref 0, event');
               Lwt.return_ok subid
             with Not_found -> Lwt.return_error "Invalid URI"
         in

         let parse_frame s =
           match Body.of_string s with
           | Error _ -> None
           | Ok v -> Msg.parse v
         in

         Websocket_cohttp_lwt.upgrade_connection
           (fst state)
           (fun frame ->
              (match frame.opcode with
               | Frame.Opcode.Close -> free_connection conn_id
               | Frame.Opcode.Ping ->
                 let frame = Frame.create
                     ~opcode:Pong
                     ~content:frame.content
                     () in
                 push_msg @@ `Frame frame
               | Frame.Opcode.Pong -> (* TODO do smth, e.g. reset the timer *) ()
               | Frame.Opcode.Text ->
                 (match parse_frame frame.content with
                  | Some (reqid, `Subscribe uri) ->
                    Lwt.async (fun () ->
                        subscribe uri
                        >>= function
                        | Ok id ->
                          let msg = Msg.compose reqid (`Subscribed id) in
                          Lwt.return @@ push_msg (`Body msg)
                        | Error error ->
                          let msg = Msg.compose reqid (`Error error) in
                          Lwt.return @@ push_msg (`Body msg))
                  | Some (reqid, `Unsubscribe id) ->
                    (match unsubscribe id with
                     | Error e -> push_msg @@ `Body (Msg.compose reqid (`Error e))
                     | Ok () ->
                       let msg = Msg.compose reqid `Unsubscribed in
                       push_msg @@ `Body msg)
                  | _ -> ())
               | _ -> ()))
         >>= fun (resp, frames_out_fn) ->

         let send msg =
           let frame = match msg with
             | `Frame x -> x
             | `Body body ->
               let msg = Body.to_string body in
               Frame.create ~content:msg () in
           frames_out_fn @@ Some frame
         in
         let event_stream =
           React.E.map send
           @@ React.E.select
             [ msg_stream
             ; React.E.map (fun () -> `Frame (Frame.create ~opcode:Ping ())) ping ] in
         store_connection conn_id (event_stream, subscriptions);

         Lwt.return resp)

  let to_http ?doc ~prefix ~ping nodes =
    let make_conn user body env (state : Api_http.state) : Api_http.answer Lwt.t =
      Lwt.return (`Instant (open_connection nodes ping user body env state))
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
