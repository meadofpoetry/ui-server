open Containers
open Zmq_lwt
open Lwt.Infix
   
type t = { ctx          : Zmq.Context.t
         ; msg          : [ `Req] Zmq.Socket.t
         ; ev           : [ `Sub ] Zmq.Socket.t
         ; sock_in      : string
         ; sock_out     : string
         ; ready        : bool ref
         ; ready_chan   : Message.chan
         ; data_chan    : Message.chan
         }

let mutex = Lwt_mutex.create ()
       
let create_sender msg_sock x =
  Lwt.catch  (fun () ->
      Lwt_unix.with_timeout 10.0 (fun () ->
          Socket.send msg_sock (Yojson.Safe.to_string x) >>= fun () ->
          (* Logs.debug (fun m -> m "(Pipeline) <send> msg was sent"); *)
          Socket.recv msg_sock >|= fun resp ->
          (* Logs.debug (fun m -> m "(Pipeline) <send> resp was received"); *)
          Ok (Yojson.Safe.from_string resp)))
    Lwt.return_error
       
let create ~sock_in ~sock_out =
  
  let ctx  = Zmq.Context.create () in
  let msg  = Zmq.Socket.create ctx Zmq.Socket.req in
  let ev   = Zmq.Socket.create ctx Zmq.Socket.sub in

  Zmq.Socket.bind msg sock_in;
  Zmq.Socket.connect ev sock_out;
  Zmq.Socket.subscribe ev "";

  let ready = ref false in

  let msg_sock   = Socket.of_socket msg in
  let ready_chan = Message.create_channel mutex (create_sender msg_sock) in

  let send =
    let send = create_sender msg_sock in
    fun msg -> if not !ready then Lwt.fail_with "backend is not ready"
               else send msg
  in
  let data_chan  = Message.create_channel mutex send in
  
  { ctx; msg; ev; sock_in; sock_out; ready = ref false; ready_chan; data_chan }

let finalize sock =
  (*Zmq.Socket.unsubscribe sock.ev "";*)
  Zmq.Socket.close       sock.ev;
  Zmq.Socket.close       sock.msg;
  Zmq.Context.terminate  sock.ctx

let get_recv sock =
  let ev_sock  = Socket.of_socket sock.ev in
  (fun on_recv_msg () ->
    Socket.recv ev_sock >>= fun msg ->
    Lwt.return @@ on_recv_msg (Yojson.Safe.from_string msg))

let get_chan sock = sock.data_chan

let reset sock =
  sock.ready := false
  
let on_ready sock init () =
  Logs.debug (fun m -> m "(Pipeline) is ready");
  (* TODO proper loop *)
  ignore @@ Lwt_main.run (Message.Protocol.ready sock.ready_chan ());
  sock.ready := true;
  init ()

module Ready = struct
  let name = "backend"
  let ready_of_yojson = function
    | `String "Ready" -> Ok ()
    | _               -> Error "notification Ready: bad value"

  let _ready_ev = ref None

  let next ev =
    let waiter, wakener = Lwt.task () in
    _ready_ev := Some (React.E.map (fun x -> Lwt.wakeup_later wakener x) (React.E.once ev));
    Lwt.on_cancel waiter (fun () -> match !_ready_ev with None -> () | Some ev -> React.E.stop ev);
    waiter

  let is_ready ev = next ev
end
