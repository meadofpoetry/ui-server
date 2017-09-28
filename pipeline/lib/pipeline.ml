open Lwt_zmq
open Lwt_react
open Lwt.Infix
open Containers

let (%) = CCFun.(%)

type content = Streams of Streams.t
             | Settings of Settings.t
             | Graph of Graph.t
             | Wm of Wm.t

type state = { ctx         : ZMQ.Context.t
             ; msg         : [ `Req] ZMQ.Socket.t
             ; ev          : [ `Sub] ZMQ.Socket.t
             ; sock_events : unit E.t
             ; db_events   : unit E.t
             }
           
type pipe = { set             : content list -> unit Lwt.t
            ; get             : [ `Streams | `Settings | `Graph | `Wm ] list -> content list Lwt.t
            ; streams_events  : Streams.t E.t
            ; settings_events : Settings.t E.t
            ; graph_events    : Graph.t E.t
            ; wm_events       : Wm.t E.t
            ; data_events     : Data.t E.t
            ; state           : state
            }

let content_to_pair input_to_s = function
  | Streams s  -> ("streams", Streams_conv.streams_to_yojson input_to_s s)
  | Settings s -> ("settings", Settings.to_yojson s)
  | Graph g    -> ("graph", Graph.to_yojson g)
  | Wm w       -> ("wm", Wm.to_yojson w)

let content_of_pair s_to_input (k,v) =
  let open Result in
  match k, v with
  | ("streams", o)  -> Streams_conv.streams_of_yojson s_to_input o  >>= fun o -> Ok (Streams o)
  | ("settings", s) -> Settings.of_yojson s >>= fun s -> Ok (Settings s)
  | ("graph", g)    -> Graph.of_yojson g    >>= fun g -> Ok (Graph g)
  | ("wm", w)       -> Wm.of_yojson w       >>= fun w -> Ok (Wm w)
  | _               -> Error ("wronge key: " ^ k)

let label = function
  | `Streams  -> "streams"
  | `Settings -> "settings"
  | `Graph    -> "graph"
  | `Wm       -> "wm"

let split_events s_to_input =
  let events, epush   = E.create () in
  let strm, strm_push = E.create () in
  let sets, sets_push = E.create () in
  let grap, grap_push = E.create () in
  let wm  , wm_push   = E.create () in
  let data, data_push = E.create () in
  let (<||>) f result = if Result.is_ok result then f (Result.get_exn result) else () in
  let split = function
    | `Assoc [("streams", tl)]  -> strm_push <||> Streams_conv.streams_of_yojson s_to_input tl
    | `Assoc [("settings", tl)] -> sets_push <||> Settings.of_yojson tl
    | `Assoc [("graph", tl)]    -> grap_push <||> Graph.of_yojson tl
    | `Assoc [("wm", tl)]       -> wm_push   <||> Wm.of_yojson tl
    | `Assoc [("data", tl)]     -> data_push <||> Data.of_yojson tl
    | _ -> ()
  in
  let events = E.map split events in
  events, epush, strm, sets, grap, wm, data

let set sock input_to_s (conv : Api.Msg_conv.converter) lst =
  let rec build = function
    | [] -> []
    | x::tl -> (content_to_pair input_to_s x)::(build tl)
  in let pairs = build lst in
  Socket.send sock (conv.to_string (`Assoc ["set", `Assoc pairs]))
  >>= fun () -> Socket.recv sock
  >>= Lwt_io.printf "Send result: %s\n"

let get sock s_to_input (conv : Api.Msg_conv.converter) keys =
  let keys = `List (List.map (fun k -> `String (label k)) keys) in
  Socket.send sock (conv.to_string (`Assoc ["get", keys]))
  >>= fun () -> Socket.recv sock
  >>= (fun js ->
    conv.of_string js
    |> function
      | `Assoc ["ok", `Assoc kvs] -> List.fold_left (fun acc x ->
                                         Result.(acc >>= fun l ->
                                                 content_of_pair s_to_input x >|= fun x -> x :: l))
                                                    (Ok []) kvs
                                     |> (function Ok v -> Lwt.return v | Error e -> Lwt.fail_with e)
      | `Assoc ["error", `String msg] -> Lwt.fail_with msg
      | _ -> Lwt.fail_with ("unknown resp: " ^ js))
              

module PSettings = struct
  type format = [ `Json | `Msgpack ]
  let format_of_yojson = function `String "json" -> Ok `Json | `String "msgpack" -> Ok `Msgpack | _ -> Error "Wrong fmt"
  let format_to_yojson = function `Json -> `String "json" | `Msgpack -> `String "msgpack"
  let format_to_string = function `Json -> "json" | `Msgpack -> "msgpack"
  type t = { bin_name  : string
           ; bin_path  : string
           ; sources   : string list
           ; msg_fmt   : format
           ; sock_in   : string
           ; sock_out  : string
           } [@@deriving yojson]
  let default = { bin_name  = "ats3-backend"
                ; bin_path  = "/home/freyr/Documents/soft/dev/ats-analyzer/build/"
                ; sources   = ["udp://224.1.2.2:1234"]
                ; msg_fmt   = `Json
                ; sock_in   = "ipc:///tmp/ats_qoe_in"
                ; sock_out  = "ipc:///tmp/ats_qoe_out"
                }
  let domain = "pipeline"
end

module Conf = Storage.Config.Make(PSettings)

module Storage : sig
  type _ req =
    | Store_streams : Streams.t -> unit Lwt.t req
    
  include (Storage.Database.STORAGE with type 'a req := 'a req)
end = Pipeline_storage
       
type t = pipe

let connect_db streams_events dbs =
  Lwt_main.run @@ Storage.init dbs;
  E.map_s (fun s -> Storage.request dbs (Storage.Store_streams s)) streams_events

let create config dbs _ =
  let cfg = Conf.get config in
  let converter = Api.Msg_conv.get_converter cfg.msg_fmt in
  let exec_path = (Filename.concat cfg.bin_path cfg.bin_name) in
  let exec_opts = Array.of_list (cfg.bin_name :: "-m" :: (PSettings.format_to_string cfg.msg_fmt) :: cfg.sources) in
  match Unix.fork () with
  | -1   -> failwith "Ooops, fork failed"
  | 0    -> Unix.execv exec_path exec_opts
  | pid  ->     
     let ctx = ZMQ.Context.create () in
     let msg = ZMQ.Socket.create ctx ZMQ.Socket.req in
     let ev  = ZMQ.Socket.create ctx ZMQ.Socket.sub in

     ZMQ.Socket.connect msg cfg.sock_in;
     ZMQ.Socket.connect ev cfg.sock_out;
     ZMQ.Socket.subscribe ev "";

     let msg_sock = Socket.of_socket msg in
     let ev_sock  = Socket.of_socket ev in

     let input_to_s = int_of_string in
     let s_to_input = string_of_int in

     let sock_events, epush,
         streams_events,
         settings_events,
         graph_events,
         wm_events,
         data_events = split_events s_to_input
     in
     let set = set msg_sock input_to_s converter in
     let get = get msg_sock s_to_input converter in
     let db_events = connect_db streams_events dbs in
     let obj = {set; get; streams_events;
                settings_events; graph_events;
                wm_events; data_events;
                state = { ctx; msg; ev; sock_events; db_events } }
     in
     (* polling loop *)
     let rec loop () =
       Socket.recv ev_sock
       >>= fun msg ->
       epush (converter.of_string msg);
       loop ()
     in
     (* finalizer *)
     let fin () =
       Lwt_unix.waitpid [] pid >>= fun _ ->
       Lwt.fail_with "Child'd died for some reason"
     in
     obj, Lwt.pick [loop (); fin ()]

let finalize pipe =
  print_endline "closing pipe";
  ZMQ.Socket.unsubscribe pipe.state.ev "";
  ZMQ.Socket.close       pipe.state.ev;
  ZMQ.Socket.close       pipe.state.msg;
  ZMQ.Context.terminate  pipe.state.ctx
