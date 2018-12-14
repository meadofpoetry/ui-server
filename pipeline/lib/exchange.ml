open Containers
open Zmq_lwt
open Lwt.Infix
open Msg_conv
open Interop
   
module Conn = struct
  type t = string [@@deriving yojson]
  let name = "connection"
end

module Conn_request = Message.Make_request(Conn)
   
type t = { ctx          : Zmq.Context.t
         ; msg          : [ `Req] Zmq.Socket.t
         ; ev           : [ `Sub ] Zmq.Socket.t
         ; sock_in      : string
         ; sock_out     : string
         ; chan         : string request
         ; ready        : bool ref
         }

let create (type a) typ (conv : a converter) ~sock_in ~sock_out =
  let create_sender msg_sock =
    let mutex = Lwt_mutex.create () in
    fun x ->
    Lwt_mutex.with_lock mutex (fun () ->
        Lwt.catch
          (fun () -> Lwt_unix.with_timeout 10.0 (fun () ->
                         Socket.send msg_sock (conv.to_string x) >>= fun () ->
                         (* Logs.debug (fun m -> m "(Pipeline) <send> msg was sent"); *)
                         Socket.recv msg_sock >|= fun resp ->
                         (* Logs.debug (fun m -> m "(Pipeline) <send> resp was received"); *)
                         Ok (conv.of_string resp)))
          Lwt.return_error)
  in

  let ctx  = Zmq.Context.create () in
  let msg  = Zmq.Socket.create ctx Zmq.Socket.req in
  let ev   = Zmq.Socket.create ctx Zmq.Socket.sub in

  let msg_sock = Socket.of_socket msg in

  let send = create_sender msg_sock in
  let chan = Conn_request.create typ send in
  
  Zmq.Socket.bind msg sock_in;
  Zmq.Socket.connect ev sock_out;
  Zmq.Socket.subscribe ev "";
  
  { ctx; msg; ev; sock_in; sock_out; ready = ref false; chan }

let finalize sock =
  (*Zmq.Socket.unsubscribe sock.ev "";*)
  Zmq.Socket.close       sock.ev;
  Zmq.Socket.close       sock.msg;
  Zmq.Context.terminate  sock.ctx

let get_send_and_recv (type a) sock (conv : a converter) =
  let create_sender msg_sock =
    let mutex = Lwt_mutex.create () in
    fun x ->
    Lwt_mutex.with_lock mutex (fun () ->
        Lwt.catch
          (fun () -> Lwt_unix.with_timeout 10.0 (fun () ->
                         Socket.send msg_sock (conv.to_string x) >>= fun () ->
                         (* Logs.debug (fun m -> m "(Pipeline) <send> msg was sent"); *)
                         Socket.recv msg_sock >|= fun resp ->
                         (* Logs.debug (fun m -> m "(Pipeline) <send> resp was received"); *)
                         Ok (conv.of_string resp)))
          Lwt.return_error)
  in
  let msg_sock = Socket.of_socket sock.msg in
  let ev_sock  = Socket.of_socket sock.ev in
  let ready    = sock.ready in
  let send =
    let send = create_sender msg_sock in
    fun msg -> if not !ready then Lwt.fail_with "backend is not ready"
               else send msg
  in
  send,
  (fun on_recv_msg () ->
    Socket.recv ev_sock >>= fun msg ->
    Lwt.return @@ on_recv_msg (conv.of_string msg))
  
let reset sock =
  sock.ready := false
  
let on_ready sock init () =
  Logs.debug (fun m -> m "(Pipeline) is ready");
  (* TODO proper loop *)
  ignore @@ Lwt_main.run (sock.chan ());
  sock.ready := true;
  init ()
