open Containers
open Zmq_lwt
open Lwt_react
open Lwt.Infix
open Msg_conv
open Message
open Notif
open Qoe_errors
(*
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
 *)
        
type options = { wm         : Wm.t Storage.Options.storage
               ; structures : Structure.Structures.t Storage.Options.storage
               ; settings   : Settings.t Storage.Options.storage
               }
        
type state = { ctx          : Zmq.Context.t
             ; msg          : [ `Req] Zmq.Socket.t
             ; ev           : [ `Sub] Zmq.Socket.t
             ; options      : options
             ; srcs         : (Common.Url.t * Common.Stream.t) list ref
             ; mutable proc : Lwt_process.process_none option
             ; ready        : bool ref
             ; ready_e      : unit event
             }

type channels =
  { wm        : Wm.t channel
  ; streams   : Structure.t list channel
  ; settings  : Settings.t channel
  }

type notifs =
  { streams   : Structure.t list signal
  ; settings  : Settings.t signal
  ; wm        : Wm.t signal
  (*; graph     : Graph.t event*)
  ; vdata     : Video_data.t event (* TODO to be split by purpose later *)
  ; adata     : Audio_data.t event
  }
  
type api = { notifs    : notifs
           ; requests  : channels
           ; model     : Model.t
           }

module Wm_options = Storage.Options.Make(Wm)
module Structures_options = Storage.Options.Make(Structure.Structures)
module Settings_options = Storage.Options.Make(Settings)

module Wm_notif = Notif.Make(Wm)
module Structures_notif = Notif.Make(Structure.Structures)
module Settings_notif = Notif.Make(Settings)
(*module Graph_notif = Notif.Make(Graph)*)
module Video_data_notif = Notif.Make(Video_data)
module Audio_data_notif = Notif.Make(Audio_data)

module Wm_msg = Message.Make(Wm)
module Structure_msg = Message.Make(Structure.Structures)
module Settings_msg = Message.Make(Settings)

let create_send (type a) (typ : a typ) (conv : a converter) msg_sock : (a -> (a, exn) result Lwt.t) =
  let mutex = Lwt_mutex.create () in
  fun x ->
  Lwt_mutex.with_lock mutex (fun () ->
      Lwt.catch
        (fun () -> Lwt_unix.with_timeout 0.5 (fun () ->
                       Socket.send msg_sock (conv.to_string x) >>= fun () ->
                       (* Logs.debug (fun m -> m "(Pipeline) <Json send> msg was sent"); *)
                       Socket.recv msg_sock >|= fun resp ->
                       (* Logs.debug (fun m -> m "(Pipeline) <Json send> resp was received"); *)
                       Ok (conv.of_string resp)))
        Lwt.return_error)
                    
let settings_init typ send (options : options) =
  let set_chan = Settings_msg.create typ send in
  Lwt.ignore_result @@ (set_chan.set options.settings#get
                        >>= function  Ok ()   -> Lwt_io.printf "Settings resp: fine\n"
                                    | Error r -> Lwt_io.printf "Settings resp: %s\n" r)

let add_storages typ send options structs wm settings =
  (* TODO test this *)
  let combine_and_set name combine opt set push data =
    let nv = combine ~set:(opt#get) data in
    match nv with
    | `Kept v    ->
       Logs.debug (fun m -> m "(Pipeline) settings for %s were not changed" name);
       push v
    | `Changed v -> 
       Lwt_main.run (set v >>= function
                     | Ok ()   ->
                        Logs.debug (fun m -> m "(Pipeline) settings for %s were changed" name);
                        Lwt.return @@ push v
                     | Error e ->
                        Logs_lwt.err (fun m -> m "(Pipeline) combine and set: failed to set data %s" e))
  in
  let storage name combine opt set events =
    let s, push = React.S.create opt#get in
    let events  = Lwt_react.E.limit (fun () -> Lwt_unix.sleep 0.5) events in
    Lwt_react.E.keep @@
      Lwt_react.E.map (combine_and_set name combine opt set push) events;
    s
  in
  
  let str_chan   = Structure_msg.create typ send in
  let wm_chan    = Wm_msg.create typ send in
  let set_chan   = Settings_msg.create typ send in
  let structures = storage "Structures" Structure.Structures.combine options.structures str_chan.set structs in
  let wm         = storage "Wm" Wm.combine options.wm wm_chan.set wm in
  let settings   = storage "Settings" Settings.combine options.settings set_chan.set settings in
  structures, wm, settings
  
let create_channels
      typ
      send
      (options : options)
      (trans : Structure.structure list -> Structure.t list)
      (s_push : Structure.structure list -> unit)
      (wm_push : Wm.t -> unit)
      (set_push : Settings.t -> unit) =
  let wm_chan    = Wm_msg.create typ send in
  let str_chan   = Structure_msg.create typ send in
  let set_chan   = Settings_msg.create typ send in
  let set_with_push store push set = fun v ->
    set v >>= function
    | Error e -> Lwt.return_error e
    | Ok ()   -> store v; push v; Lwt.return_ok ()
  in
  let s_get_upd () =
    str_chan.get () >|= function
    | Error _ as r -> r
    | Ok v  -> Ok(trans v)
  in
  let wm_set_upd  = set_with_push options.wm#store wm_push wm_chan.set in
  let s_set_upd   =
    let set = set_with_push options.structures#store s_push str_chan.set in
    fun s -> set @@ Structure.Streams.unwrap s
  in
  let set_set_upd = set_with_push options.settings#store set_push set_chan.set in
  { wm       = { wm_chan with set = wm_set_upd }
  ; streams  = { get = s_get_upd; set = s_set_upd }
  ; settings = { set_chan with set = set_set_upd }
  }

let init_exchange (type a) (typ : a typ) send structures_packer options =
  let create_table lst =
    let table = Hashtbl.create 10 in
    List.iter (fun (n,f) -> Hashtbl.add table n f) lst;
    table
  in
  let unwrap f result =
    match result with
    | Ok r -> (f r : unit)
    | Error e -> Logs.err (fun m -> m "(Pipeline) notification parse error %s" e)
  in
  let events, epush     = E.create () in
  let ready, ready_push = E.create () in
  let strm, strm_push   = E.create () in
  let sets, sets_push   = E.create () in
  let wm  , wm_push     = E.create () in
  let vdata, vdata_push = E.create () in
  let adata, adata_push = E.create () in
  let table =
    create_table [ Notif.Ready.create typ (unwrap ready_push)
                 ; Wm_notif.create typ (unwrap wm_push)
                 ; Structures_notif.create typ (unwrap strm_push)
                 ; Settings_notif.create typ (unwrap sets_push)
                 ; Video_data_notif.create typ (unwrap vdata_push)
                 ; Audio_data_notif.create typ (unwrap adata_push)
      ]
  in
  let dispatch = dispatch typ table in
  Lwt_react.E.keep @@ E.map dispatch events;

  let requests =
    create_channels typ send options structures_packer strm_push wm_push sets_push
  in

  let structures, wm, settings = add_storages typ send options strm wm sets in
  let streams = S.map structures_packer structures in
  let notifs = { streams; wm; settings; adata; vdata } in
  
  epush, ready, notifs, requests
  
  
let create (type a) (typ : a typ) db_conf config sock_in sock_out =
  let stor    = Storage.Options.Conf.get config in
  let options = { wm         = Wm_options.create stor.config_dir ["pipeline";"wm"]
                ; structures = Structures_options.create stor.config_dir ["pipeline";"structures"]
                ; settings   = Settings_options.create stor.config_dir ["pipeline";"settings"]
                }
  in
  let ctx = Zmq.Context.create () in
  let msg = Zmq.Socket.create ctx Zmq.Socket.req in
  let ev  = Zmq.Socket.create ctx Zmq.Socket.sub in

  Zmq.Socket.bind msg sock_in;
  Zmq.Socket.bind ev sock_out;
  Zmq.Socket.subscribe ev "";

  let msg_sock = Socket.of_socket msg in
  let ev_sock  = Socket.of_socket ev in

  let srcs      = ref [] in
  let ready     = ref false in
  let proc      = None in
  let converter = (Msg_conv.get_converter typ) in
  
  let send =
    let send = create_send typ converter msg_sock in
    fun msg -> if not !ready
               then Lwt.fail_with "backend is not ready"
               else send msg
  in

  let merge v = Structure_conv.match_streams srcs v in
  
  let epush, ready_e, notifs, requests =
    init_exchange typ send merge options
  in

  let model = Model.create db_conf notifs.streams notifs.vdata notifs.adata in
  
  let api = { notifs; requests; model } in
  
  let state = { ctx; msg; ev;
                options; srcs; proc;
                ready; ready_e }
  in
  let recv () =
    Socket.recv ev_sock
    >>= fun msg ->
    Lwt.return @@ epush (converter.of_string msg)
  in
  api, state, recv, send

let reset typ send bin_path bin_name msg_fmt api state (sources : (Common.Url.t * Common.Stream.t) list) =
  let exec_path = (Filename.concat bin_path bin_name) in
  let msg_fmt   = Pipeline_settings.format_to_string msg_fmt in
  let uris      = List.map Fun.(fst %> Common.Url.to_string) sources in
  let exec_opts = Array.of_list (bin_name :: "-m" :: msg_fmt :: uris) in
  state.srcs  := sources;
  state.ready := false;
  Option.iter (fun proc -> proc#terminate) state.proc;
  let is_ready = Notif.is_ready state.ready_e in
  state.proc <- Some (Lwt_process.open_process_none (exec_path, exec_opts));
  (is_ready >|= fun () ->
   Logs.debug (fun m -> m "(Pipeline) is ready");
   state.ready := true;
   settings_init typ send state.options)
  |> Lwt.ignore_result;
  Model.set_streams api.model (List.map snd sources);
  Logs.debug (fun m -> m "(Pipeline) reset [%s]" (Array.fold_left (fun acc x -> acc ^ " " ^ x) "" exec_opts))
  
let finalize state =
  Zmq.Socket.unsubscribe state.ev "";
  Zmq.Socket.close       state.ev;
  Zmq.Socket.close       state.msg;
  Zmq.Context.terminate  state.ctx;
  Option.iter (fun proc -> proc#terminate) state.proc;
  state.proc <- None;
  Logs.debug (fun m -> m "(Pipeline) finalize")
