open Lwt_zmq
open Lwt_react
open Lwt.Infix

type t = { set            : (string * Yojson.Safe.json) list -> unit Lwt.t
         ; get            : string list -> (string * Yojson.Safe.json) list Lwt.t
         ; options_events : Yojson.Safe.json E.t
         ; setting_events : Yojson.Safe.json E.t
         ; graph_events   : Yojson.Safe.json E.t
         ; wm_events      : Yojson.Safe.json E.t
         ; data_events    : Yojson.Safe.json E.t
         }

let split_events events =
  let opts, opts_push = E.create () in
  let sets, sets_push = E.create () in
  let grap, grap_push = E.create () in
  let wm  , wm_push   = E.create () in
  let data, data_push = E.create () in
  
  let splt = function
    | `Assoc [("options", tl)] -> opts_push tl
    | `Assoc [("settings", tl)] -> sets_push tl
    | `Assoc [("graph", tl)] -> grap_push tl
    | `Assoc [("wm", tl)] -> wm_push tl
    | `Assoc [("data", tl)] -> data_push tl
    | _ -> ()
  in
  let _ = E.map splt events in
  opts, sets, grap, wm, data

let set sock pairs =
  Socket.send sock (Yojson.Safe.to_string (`Assoc ["set", `Assoc pairs]))
  >>= fun () -> Socket.recv sock
  >>= Lwt_io.printf "Send result: %s\n"

let get sock keys =
  let keys = `List (List.map (fun k -> `String k) keys) in
  Socket.send sock (Yojson.Safe.to_string (`Assoc ["get", keys]))
  >>= fun () -> Socket.recv sock
  >>= (fun js ->
    Yojson.Safe.from_string js
    |> function
      | `Assoc ["ok", `Assoc kvs] -> Lwt.return kvs
      | `Assoc ["error", `String msg] -> Lwt.fail_with msg
      | _ -> Lwt.fail_with ("unknown resp: " ^ js))
       
let create () =
  match Unix.fork () with
  | -1 -> failwith "Ooops, fork faild"
  | 0  -> Unix.execv "/home/freyr/Documents/soft/dev/ats-analyzer/build/ats3-backend"
                     [|"ats3-backend"; "udp://224.1.2.2:1234"; "-m"; "json"|]
  | _  ->     
     let ctx = ZMQ.Context.create () in
     let msg_sock = ZMQ.Socket.create ctx ZMQ.Socket.req in
     let ev_sock  = ZMQ.Socket.create ctx ZMQ.Socket.sub in

     ZMQ.Socket.connect msg_sock "ipc:///tmp/ats_qoe_in";
     ZMQ.Socket.connect ev_sock "ipc:///tmp/ats_qoe_out";
     ZMQ.Socket.subscribe ev_sock "";

     let msg_sock = Socket.of_socket msg_sock in
     let ev_sock  = Socket.of_socket ev_sock in

     let events, epush = E.create () in
     let options_events,
         setting_events,
         graph_events,
         wm_events,
         data_events = split_events events
     in
     let set = set msg_sock in
     let get = get msg_sock in
     let obj = {set; get; options_events;
                setting_events; graph_events;
                wm_events; data_events}
     in
     let rec loop () =
       Socket.recv ev_sock
       >>= fun msg ->
       epush (Yojson.Safe.from_string msg);
       (*Lwt_io.printf "Some %s\n" msg |> ignore;*)
       loop ()
     in
     obj, loop
     
