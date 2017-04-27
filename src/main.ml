open Lwt
open Lwt_react
open Websocket_cohttp_lwt

let listen_input () =
  let e, push = E.create () in
  let rec loop () =
    Lwt_io.read_line Lwt_io.stdin
    >>= fun line ->
    push line; loop ()
  in
  e, loop

let create_handler event =
  let handler
        (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
        (req  : Cohttp_lwt_unix.Request.t)
        (body : Cohttp_lwt_body.t) =
    let open Frame in
    Lwt_io.eprintf "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn)
    >>= fun _ ->
    let uri = Cohttp.Request.uri req in
    match Uri.path uri with
    | "/" -> Lwt_io.eprintf "[PATH] /\n%!"
             >>= fun () ->
             Cohttp_lwt_unix.Server.respond_string
               ~status:`OK
               ~body: {| 
                       <html>
                       <head>
                       <meta charset="utf-8">
                       <script src="//code.jquery.com/jquery-1.11.3.min.js"></script>
                       <script>
                       $(window).on('load', function(){
                       ws = new WebSocket('ws://localhost:7777/ws');
                       ws.onmessage = function(x) {
                       $('#msg').html("<p>" + x.data + "</p>");
                       };
                       });
                       </script>
                       </head>
                       <body>
                       <div id='msg'></div>
                       </body>
                       </html>
                       |}
               ()
    | "/ws" -> Lwt_io.eprintf "[PATH] /ws\n%!"
               >>= fun () ->
               Cohttp_lwt_body.drain_body body
               >>= fun () ->
               Websocket_cohttp_lwt.upgrade_connection req (fst conn) (
                                                         fun f ->
                                                         match f.opcode with
                                                         | Opcode.Close -> Printf.eprintf "[RECV] CLOSE\n%!"
                                                         | _ -> Printf.eprintf "[RECV] %s\n%!" f.content
                                                       )
               >>= fun (resp, body, frames_out_fn) ->
               (* send a message to the client every second *)
               let send line =
                 frames_out_fn @@ Some (of_bytes @@ BytesLabels.of_string @@ line)
               in
               let _ = E.map send event
               in
               Lwt.return (resp, (body :> Cohttp_lwt_body.t))
    | _ -> Lwt_io.eprintf "[PATH] Catch-all\n%!"
           >>= fun () ->
           Cohttp_lwt_unix.Server.respond_string
             ~status:`Not_found
             ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
             ()
  in
  handler

let start_server host port event () =
  let conn_closed (ch,_) =
    Printf.eprintf "[SERV] connection %s closed\n%!"
                   (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  Lwt_io.eprintf "[SERV] Listening for HTTP on port %d\n%!" port >>= fun () ->
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback:(create_handler event) ~conn_closed ())

(* main *)
let () =
  let e, eloop = listen_input () in
  Lwt_main.run @@
    Lwt.choose [(start_server "localhost" 7777 e ());
                (eloop ())
               ]
