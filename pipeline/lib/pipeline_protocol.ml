open Containers
open Lwt_zmq
open Lwt_react
open Lwt.Infix
open Msg_conv
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

let settings_init typ send (options : options) =
  let set_chan = Settings_msg.create typ send in
  Lwt.ignore_result @@ (set_chan.set options.settings#get
                        >>= function  Ok ()   -> Lwt_io.printf "Settings resp: fine\n"
                                    | Error r -> Lwt_io.printf "Settings resp: %s\n" r)
  
(* TODO test this *)
let combine_and_set combine opt set push data =
  let nv = combine ~set:(opt#get) data in
  match nv with
  | `Kept v    -> push v
  | `Changed v -> 
     Lwt_main.run (set v >>= function
                   | Ok ()   -> Lwt.return @@ push v
                   | Error e -> Lwt_io.printf "combine and set: failed to set data %s\n" e)

let storage combine opt set events =
  let s, push = React.S.create opt#get in
  let events  = limit (fun () -> Lwt_unix.sleep 0.5) events in
  Lwt_react.E.keep @@
    Lwt_react.E.map (combine_and_set combine opt set push) events;
  s

let add_storages typ send options structs wm settings =
  let str_chan   = Structure_msg.create typ send in
  let wm_chan    = Wm_msg.create typ send in
  let set_chan   = Settings_msg.create typ send in
  let structures = storage Structure.Structures.combine options.structures str_chan.set structs in
  let wm         = storage Wm.combine options.wm wm_chan.set wm in
  let settings   = storage Settings.combine options.settings set_chan.set settings in
  structures, wm, settings
  
let notif_events typ =
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
    [ Wm_notif.create typ ((<$>) wm_push)
    ; Structures_notif.create typ ((<$>) strm_push)
    ; Settings_notif.create typ ((<$>) sets_push)
    ; Graph_notif.create typ ((<$>) grap_push)
    ; Video_data_notif.create typ ((<$>) vdata_push)
    ; Audio_data_notif.create typ ((<$>) adata_push)
    ];

  let dispatch = dispatch typ table in
  Lwt_react.E.keep @@ E.map dispatch events;
  epush, strm, sets, grap, wm, vdata, adata,
  strm_push, wm_push, sets_push
  
let create_channels
      typ
      send
      (options : options)
      (trans : Structure.structure list -> Structure.t list)
      (s_push : Structure.structure list -> unit)
      (wm_push : Wm.t -> unit) =
  let wm_chan    = Wm_msg.create typ send in
  let str_chan   = Structure_msg.create typ send in
  let set_chan   = Settings_msg.create typ send in
  let s_get_upd () =
    str_chan.get () >|= function
    | Error _ as r -> r
    | Ok v  -> Ok(trans v)
  in
  let wm_set_upd wm = options.wm#store wm; wm_chan.set wm in
  let s_set_upd s =
    let s = Structure.Streams.unwrap s in
    options.structures#store s;
    str_chan.set s
  in
  let set_set_upd s = options.settings#store s; set_chan.set s in
  { wm       = { wm_chan with set = wm_set_upd }
  ; streams  = { get = s_get_upd; set = s_set_upd }
  ; settings = { set_chan with set = set_set_upd }
  }

let create_send (type a) (typ : a typ) (conv : a converter) msg_sock : (a -> a Lwt.t) =
  fun x ->
  Socket.send msg_sock (conv.to_string x) >>= fun () ->
  Socket.recv msg_sock >|= fun resp ->
  conv.of_string resp
  
let create (type a) (typ : a typ) config sock_in sock_out hardware_streams =
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

  let converter = (Msg_conv.get_converter typ) in
  let send = create_send typ converter msg_sock in
  settings_init typ send options;
  
  let epush,
      structures, settings,
      graph, wm,
      vdata, adata,
      strms_push, wm_push, sets_push = notif_events typ
  in
  let structures, wm, settings = add_storages typ send options structures wm settings in
  
  let streams = S.l2
                  (Structure_conv.match_streams Common.Topology.(Some { input = TSOIP; id = 42 }))
                  hardware_streams structures
  in
  let requests =
    let merge s =
      (Structure_conv.match_streams Common.Topology.(Some { input = TSOIP; id = 42 }))
        (S.value hardware_streams) s
    in
    create_channels typ send options merge strms_push wm_push
  in
  
  let api = { streams
            ; settings; graph; wm; vdata; adata
            ; requests
            } in
  
  let state = { ctx; msg; ev; options } in
  let recv () =
    Socket.recv ev_sock
    >>= fun msg ->
    Lwt.return @@ epush (converter.of_string msg)
  in
  api, state, recv

  
let finalize state =
  ZMQ.Socket.unsubscribe state.ev "";
  ZMQ.Socket.close       state.ev;
  ZMQ.Socket.close       state.msg;
  ZMQ.Context.terminate  state.ctx
