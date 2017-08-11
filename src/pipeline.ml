open Lwt_zmq
open Lwt_react
open Lwt.Infix
open Containers
open Common

type content = Options of Options.t
             | Settings of Settings.t
             | Graph of Graph.t
             | Wm of Wm.t
   
type pipe = { set             : content list -> unit Lwt.t
            ; get             : [ `Options | `Settings | `Graph | `Wm ] list -> content list Lwt.t
            ; options_events  : Options.t E.t
            ; settings_events : Settings.t E.t
            ; graph_events    : Graph.t E.t
            ; wm_events       : Wm.t E.t
            ; data_events     : Data.t E.t
            }

let content_to_pair = function
  | Options o  -> ("options", Options.to_yojson o)
  | Settings s -> ("settings", Settings.to_yojson s)
  | Graph g    -> ("graph", Graph.to_yojson g)
  | Wm w       -> ("wm", Wm.to_yojson w)

let content_of_pair (k,v) =
  let open Result in
  match k, v with
  | ("options", o)  -> Options.of_yojson o  >>= fun o -> Ok (Options o)
  | ("settings", s) -> Settings.of_yojson s >>= fun s -> Ok (Settings s)
  | ("graph", g)    -> Graph.of_yojson g    >>= fun g -> Ok (Graph g)
  | ("wm", w)       -> Wm.of_yojson w       >>= fun w -> Ok (Wm w)
  | _               -> Error ("wronge key: " ^ k)

let label = function
  | `Options  -> "options"
  | `Settings -> "settings"
  | `Graph    -> "graph"
  | `Wm       -> "wm"

let split_events events =
  let opts, opts_push = E.create () in
  let sets, sets_push = E.create () in
  let grap, grap_push = E.create () in
  let wm  , wm_push   = E.create () in
  let data, data_push = E.create () in
  let (<||>) f result = if Result.is_ok result then f (Result.get_exn result) else () in
  let split = function
    | `Assoc [("options", tl)]  -> opts_push <||> Options.of_yojson tl
    | `Assoc [("settings", tl)] -> sets_push <||> Settings.of_yojson tl
    | `Assoc [("graph", tl)]    -> grap_push <||> Graph.of_yojson tl
    | `Assoc [("wm", tl)]       -> wm_push   <||> Wm.of_yojson tl
    | `Assoc [("data", tl)]     -> data_push <||> Data.of_yojson tl
    | _ -> ()
  in
  let _ = E.map split events in
  opts, sets, grap, wm, data

let set sock (conv : Msg_conv.converter) lst =
  let rec build = function
    | [] -> []
    | x::tl -> (content_to_pair x)::(build tl)
  in let pairs = build lst in
  Socket.send sock (conv.to_string (`Assoc ["set", `Assoc pairs]))
  >>= fun () -> Socket.recv sock
  >>= Lwt_io.printf "Send result: %s\n"

let get sock (conv : Msg_conv.converter) keys =
  let keys = `List (List.map (fun k -> `String (label k)) keys) in
  Socket.send sock (conv.to_string (`Assoc ["get", keys]))
  >>= fun () -> Socket.recv sock
  >>= (fun js ->
    conv.of_string js
    |> function
      | `Assoc ["ok", `Assoc kvs] -> List.fold_left (fun acc x -> Result.(acc >>= fun l ->
                                                                          content_of_pair x >|= fun x -> x :: l))
                                                    (Ok []) kvs
                                     |> (function Ok v -> Lwt.return v | Error e -> Lwt.fail_with e)
      | `Assoc ["error", `String msg] -> Lwt.fail_with msg
      | _ -> Lwt.fail_with ("unknown resp: " ^ js))
              

module Settings = struct
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

module Conf = Config.Make(Settings)
module Storage : sig
  type _ req =
    | Store_opts : Options.t -> int64 Lwt.t req
  include (Database.STORAGE with type 'a req := 'a req)
end = Pipeline_storage
       
type t = pipe

let connect_db pipe dbs =
  E.map_p (fun o -> Storage.request dbs (Store_opts o)) pipe.options_events

let create config dbs =
  let cfg = Conf.get config in
  let converter = Msg_conv.get_converter cfg.msg_fmt in
  let exec_path = (Filename.concat cfg.bin_path cfg.bin_name) in
  let exec_opts = Array.of_list (cfg.bin_name :: "-m" :: (Settings.format_to_string cfg.msg_fmt) :: cfg.sources) in
  match Unix.fork () with
  | -1 -> failwith "Ooops, fork faild"
  | 0  -> Unix.execv exec_path exec_opts
  | _  ->     
     let ctx = ZMQ.Context.create () in
     let msg_sock = ZMQ.Socket.create ctx ZMQ.Socket.req in
     let ev_sock  = ZMQ.Socket.create ctx ZMQ.Socket.sub in

     ZMQ.Socket.connect msg_sock cfg.sock_in;
     ZMQ.Socket.connect ev_sock cfg.sock_out;
     ZMQ.Socket.subscribe ev_sock "";

     let msg_sock = Socket.of_socket msg_sock in
     let ev_sock  = Socket.of_socket ev_sock in

     let events, epush = E.create () in
     let options_events,
         settings_events,
         graph_events,
         wm_events,
         data_events = split_events events
     in
     let set = set msg_sock converter in
     let get = get msg_sock converter in
     let obj = {set; get; options_events;
                settings_events; graph_events;
                wm_events; data_events}
     in
     connect_db obj dbs |> ignore;
     let rec loop () =
       Socket.recv ev_sock
       >>= fun msg ->
       epush (converter.of_string msg);
       (*Lwt_io.printf "Some %s\n" msg |> ignore;*)
       loop ()
     in
     obj, (loop ())

let finalize : t -> unit = fun _ -> ()
