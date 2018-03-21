open Containers
open Lwt_zmq
open Lwt_react
open Lwt.Infix
open Message
open Notif

let limit f e =
  let limiter = ref Lwt.return_unit in
  let delayed = ref None in
  let event, push = React.E.create () in
  let iter =
    React.E.fmap
      (fun x ->
        if Lwt.is_sleeping !limiter then begin
            match !delayed with
            | Some cell ->
               cell := x;
               None
            | None ->
               let cell = ref x in
               delayed := Some cell;
               None
          end else begin
            limiter := f ();
            push x;
            None
          end)
      e
  in
  React.E.select [iter; event]
   
let (%) = Fun.(%)
(* TODO make 'active type label *)

type options = { wm         : Wm.t Storage.Options.storage
               ; structures : Structure.Structures.t Storage.Options.storage
               ; settings   : Settings.t Storage.Options.storage
               }
        
type state = { ctx         : ZMQ.Context.t
             ; msg         : [ `Req] ZMQ.Socket.t
             ; ev          : [ `Sub] ZMQ.Socket.t
             ; sock_events : unit event
                                  (*; _e : unit event*)
             ; options     : options
             }

type channels =
  { wm        : Wm.t channel
  ; streams   : Structure.t list channel
  ; settings  : Settings.t channel
  }
  
type api = { streams   : Structure.t list signal
           ; settings  : Settings.t signal
           ; graph     : Graph.t event
           ; wm        : Wm.t signal
           ; vdata     : Video_data.t event (* TODO to be split by purpose later *)
           ; adata     : Audio_data.t event
           ; requests  : channels
           }

module Wm_options = Storage.Options.Make(Wm)
module Structures_options = Storage.Options.Make(Structure.Structures)
module Settings_options = Storage.Options.Make(Settings)

module Wm_notif = Notif.Make(Wm)
module Structures_notif = Notif.Make(Structure.Structures)
module Settings_notif = Notif.Make(Settings)
module Graph_notif = Notif.Make(Graph)
module Video_data_notif = Notif.Make(Video_data)
module Audio_data_notif = Notif.Make(Audio_data)

module Wm_msg = Message.Make(Wm)
module Structure_msg = Message.Make(Structure.Structures)
module Settings_msg = Message.Make(Settings)

(* TODO test this *)
let combine_and_set combine opt set push data =
  let nv = combine ~set:(opt#get) data in
  match nv with
  | `Kept v    -> push v
  | `Changed v -> 
     Lwt_main.run (set v >>= function
                   | Ok ()   -> Lwt.return @@ push v
                   | Error e -> Lwt_io.printf "combine and set: failed to set data %s\n" e)

let combiner combine opt set events =
  let s, push = React.S.create opt#get in
  let events  = limit (fun () -> Lwt_unix.sleep 0.5) events in
  Lwt_react.E.keep @@
    Lwt_react.E.map (combine_and_set combine opt set push) events;
  s

let create_combiners send options structs wm settings =
  let _, s_set  = Structure_msg.create send `Json in
  let _, wm_set = Wm_msg.create send `Json in
  let _, se_set = Settings_msg.create send `Json in
  let structures = combiner Structure.Structures.combine options.structures s_set structs in
  let wm         = combiner Wm.combine options.wm wm_set wm in
  let settings   = combiner Settings.combine options.settings se_set settings in
  structures, wm, settings
  
let notif_events typ =
  let _ = Lwt_io.printf "split_events\n" |> ignore in
  let events, epush     = E.create () in
  let strm, strm_push   = E.create () in
  let sets, sets_push   = E.create () in
  let grap, grap_push   = E.create () in
  let wm  , wm_push     = E.create () in
  let vdata, vdata_push = E.create () in
  let adata, adata_push = E.create () in  
  let (<$>) f result =
    match result with
    | Ok r -> (f r : unit)
    | Error e -> Lwt_io.printf "parse error %s\n" e |> ignore in

  let table = Hashtbl.create 10 in
  List.iter (fun (n,f) -> Hashtbl.add table n f)
    [ Wm_notif.create `Json ((<$>) wm_push)
    ; Structures_notif.create `Json ((<$>) strm_push)
    ; Settings_notif.create `Json ((<$>) sets_push)
    ; Graph_notif.create `Json ((<$>) grap_push)
    ; Video_data_notif.create `Json ((<$>) vdata_push)
    ; Audio_data_notif.create `Json ((<$>) adata_push)
    ];

  let dispatch = dispatch_js table in
  let events = E.map dispatch events in
  events, epush,
  strm, sets, grap, wm, vdata, adata,
  strm_push, wm_push
  
let create_channels
      typ
      send
      (options : options)
      (trans : Structure.structure list -> Structure.t list)
      (s_push : Structure.structure list -> unit)
      (wm_push : Wm.t -> unit) =
  let wm_get, wm_set   = Wm_msg.create send `Json in
  let s_get, s_set     = Structure_msg.create send `Json in
  let set_get, set_set = Settings_msg.create send `Json in
  let s_get_upd () =
    s_get () >|= function
    | Error _ as r -> r
    | Ok v  -> Ok(trans v)
  in
  let wm_set_upd wm = options.wm#store wm; wm_set wm in
  let s_set_upd s =
    let s = Structure.Streams.unwrap s in
    options.structures#store s;
    s_set s
  in
  let set_set_upd s = options.settings#store s; set_set s in
  { wm       = { get = wm_get; set = wm_set_upd }
  ; streams  = { get = s_get_upd; set = s_set_upd }
  ; settings = { get = set_get; set = set_set_upd }
  }
  
let create config sock_in sock_out hardware_streams =
  let stor    = Storage.Options.Conf.get config in
  let options = { wm         = Wm_options.create stor.config_dir ["pipeline";"wm"]
                ; structures = Structures_options.create stor.config_dir ["pipeline";"structures"]
                ; settings   = Settings_options.create stor.config_dir ["pipeline";"settings"]
                }
  in
  let ctx = ZMQ.Context.create () in
  let msg = ZMQ.Socket.create ctx ZMQ.Socket.req in
  let ev  = ZMQ.Socket.create ctx ZMQ.Socket.sub in

  ZMQ.Socket.connect msg sock_in;
  ZMQ.Socket.connect ev sock_out;
  ZMQ.Socket.subscribe ev "";

  let msg_sock = Socket.of_socket msg in
  let ev_sock  = Socket.of_socket ev in

  let send_js = fun x ->
    Socket.send msg_sock (Yojson.Safe.to_string x) >>= fun () ->
    Socket.recv msg_sock >|= fun js ->
    Yojson.Safe.from_string js
  in
  
  let sock_events, epush,
      structures, settings,
      graph, wm,
      vdata, adata,
      strms_push, wm_push = notif_events ()
  in
  let structures, wm, settings = create_combiners send_js options structures wm settings in
  let streams = S.l2
                  (Structure_conv.match_streams Common.Topology.(Some { input = TSOIP; id = 42 }))
                  hardware_streams structures
  in
  let merge = fun s -> (Structure_conv.match_streams Common.Topology.(Some { input = TSOIP; id = 42 })) (S.value hardware_streams) s in
  let requests = create_channels `Json send_js options merge strms_push wm_push in
  (*let set = set msg_sock converter in
  let get = fun x -> get msg_sock converter structure wm settings x in *)
  let api = { streams
            ; settings; graph; wm; vdata; adata
            ; requests
            } in
  (* TODO proper init *)
  let _, set_set = Settings_msg.create send_js `Json in
  Lwt.ignore_result @@ (set_set options.settings#get
                        >>= function  Ok () -> Lwt_io.printf "Settings resp: fine\n"
                                    | Error r -> Lwt_io.printf "Settings resp: %s\n" r);
  (*let _e  = Lwt_react.E.map_p (fun x ->
                Yojson.Safe.pretty_to_string x
                |> Lwt_io.printf "Video_data: %s\n") msgs in*)
  let state = { ctx; msg; ev; sock_events; options } in
  let recv () =
    Socket.recv ev_sock
    >>= fun msg ->
    Lwt.return @@ epush (Yojson.Safe.from_string msg)
  in
  api, state, recv

  
let finalize state =
  ZMQ.Socket.unsubscribe state.ev "";
  ZMQ.Socket.close       state.ev;
  ZMQ.Socket.close       state.msg;
  ZMQ.Context.terminate  state.ctx
