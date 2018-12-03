open Containers
open Lwt.Infix
open Message
open Notif
open Qoe_errors
open Common

type options =
  { wm : Wm.t Storage.Options.storage
  ; structures : Structure.t list Storage.Options.storage
  ; settings : Settings.t Storage.Options.storage
  }

type state =
  { socket : Exchange.t
  ; ready_e : unit React.event
  ; mutable proc : Lwt_process.process_none option
  }

type notifs =
  { streams : Structure.t list React.signal
  (*; settings : Settings.t React.signal*)
  ; wm : Wm.t React.signal
  ; applied_structs : Structure.t list React.signal
  ; status : Qoe_status.t list React.signal
  ; vdata : Video_data.t React.event (* TODO to be split by purpose later *)
  ; adata : Audio_data.t React.event
  }

type api =
  { notifs  : notifs
  ; options : options
  ; sources : (Url.t * Stream.t) list ref
  ; channel : Message.chan
  ; model   : Model.t
  }

module Wm_options = Storage.Options.Make(Wm)
module Structures_options = Storage.Options.Make(Structure.Many)
module Settings_options = Storage.Options.Make(Settings)

(*
let settings_init typ send (options : options) =
  let set_chan = Settings_msg.create typ send in
  Lwt.ignore_result @@ (set_chan.set options.settings#get
                        >>= function  Ok ()   -> Lwt_io.printf "Settings resp: fine\n"
                                    | Error r -> Lwt_io.printf "Settings resp: %s\n" r)
 *)

let limit_inert ~eq t s =
  let open React in
  let tm = ref Lwt.return_unit in
  let event, epush = E.create () in
  let iter = E.fmap (fun x ->
                 if Lwt.is_sleeping !tm then begin
                     Lwt.cancel !tm;
                     tm := Lwt_unix.sleep t;
                     Lwt.on_success !tm (fun () -> epush x);
                     None end
                 else begin
                     tm := Lwt_unix.sleep t;
                     Lwt.on_success !tm (fun () -> epush x);
                     None
                   end) (S.changes s)
  in S.hold ~eq (S.value s) (E.select [iter; event])

let pid_equal = fun (a, b, c) (e, f, g) ->
  Stream.ID.equal a e
  && Int.equal b f
  && Int.equal c g

let pid_diff post prev =
  let is_in pid = List.exists (pid_equal pid)
  in `Diff (object
           method appeared = List.filter (fun x -> not @@ is_in x prev) post
           method disappeared = List.filter (fun x -> not @@ is_in x post) prev
         end)
   
let update_status (lst : Qoe_status.t list) event =
  let pid_to_status (stream,channel,pid) : Qoe_status.t =
    { stream; channel; pid; playing = true }
  in
  let rec apply_status (entry : Qoe_status.t) : Qoe_status.t list -> Qoe_status.t list = function
    | [] -> []
    | h::tl ->
       if Stream.ID.equal h.stream entry.stream
          && h.channel = entry.channel
          && h.pid = entry.pid
       then entry::tl
       else h::(apply_status entry tl)
  in
  match event with
  | `Diff o ->
     let new_lst = List.filter (fun (x : Qoe_status.t) ->
                       not @@ List.exists (fun (s,c,p) ->
                                  Stream.ID.equal s x.stream && c = x.channel && p = x.pid)
                                o#disappeared) lst
     in (List.map pid_to_status o#appeared) @ new_lst
  | `Status s -> apply_status s lst
   
let notification_mutex = Lwt_mutex.create ()
                       
let notification_attach_setter
      ~(combine: set:'a -> 'b -> [< `Kept of 'a | `Changed of 'a | `Updated of 'a ])
      ~options
      ~set
      ~(signal:'b React.signal) =  
  let make_setter merge set value =
    match merge value with
    | `Kept v -> Lwt.return_some v
    | `Updated v -> (options#store v; Lwt.return_some v)
    | `Changed v -> Lwt.try_bind (fun () -> set v)
                      (function Ok () -> Lwt.return_some v
                              | Error _e -> Lwt.return_none (* TODO add log *))
                      (function Failure _e -> Lwt.return_none)
  in                   
  let signal_add_setter signal default setter =
    let signal = limit_inert ~eq:Pervasives.(=) 2.0 signal in
    React.S.fmap_s ~eq:Pervasives.(=) setter default signal
    |> Lwt_main.run
  in
  let merge v = Lwt_mutex.with_lock notification_mutex
                  (fun () -> make_setter (fun (x : 'b) ->
                                 combine ~set:(options#get) x) set v)
  in
  let default = options#get in
  signal_add_setter signal default merge
  
let create_notifications (options : options) channel =
  let open Notif in
  let ready  = add_event ~name:Exchange.Ready.name Exchange.Ready.ready_of_yojson in
  (* TODO remove Equal.poly *)
  let status = add_event ~name:"stream_lost" Qoe_status.of_yojson in
  let vdata  = add_event ~name:"video_data" Qoe_errors.Video_data.of_yojson in
  let adata  = add_event ~name:"audio_data" Qoe_errors.Audio_data.of_yojson in
  let wm     = add_signal ~name:"wm" ~eq:Wm.equal ~init:Wm.default Wm.of_yojson in
  let streams = add_signal ~name:"stream_parser" ~eq:Equal.poly ~init:[]
                  Structure.Many.of_yojson in
  let applied_structs = add_signal ~name:"applied_streams" ~eq:Equal.poly ~init:[]
                          Structure.Many.of_yojson in
  
  let applied_structs = notification_attach_setter
                          ~combine:Structure.combine
                          ~set:(Message.Protocol.graph_apply_structure channel)
                          ~options:(options.structures)
                          ~signal:(React.S.l2 ~eq:Pervasives.(=) Pair.make
                                     applied_structs
                                     streams)
  in
  let wm = notification_attach_setter
             ~combine:Wm.combine
             ~set:(Message.Protocol.wm_apply_layout channel)
             ~options:(options.wm)
             ~signal:wm
  in

  let pids_diff =
    applied_structs
    |> React.S.map ~eq:(Equal.list pid_equal) (fun x -> Structure.pids x)
    |> React.S.diff pid_diff
  in
  
  let status =
    React.E.select [pids_diff; React.E.map (fun x -> `Status x) status]
    |> React.S.fold ~eq:(fun _ _ -> false) update_status []
  in
  
  let notifs = { streams; applied_structs; wm; vdata; adata; status } in 
  ready, notifs
  
let create db_conf config sock_in sock_out =
  let stor    = Storage.Options.Conf.get config in
  let options =
    { wm = Wm_options.create stor.config_dir ["pipeline";"wm"]
    ; structures = Structures_options.create stor.config_dir ["pipeline";"structures"]
    ; settings = Settings_options.create stor.config_dir ["pipeline";"settings"]
    } in
  let sources = ref [] in
  let proc    = None in

  let socket  = Exchange.create ~sock_in ~sock_out in
  let recv    = Exchange.get_recv socket in
  let channel = Exchange.get_chan socket in

  (*let merge v = Structure_conv.match_streams srcs v in*)

  let ready_e, notifs =
    create_notifications options channel in

  let model = Model.create db_conf notifs.streams notifs.status notifs.vdata notifs.adata in
  let api =
    { notifs; options; sources; channel; model }
  in
  let state =
    { socket; ready_e; proc }
  in
  api, state, (recv Notif.dispatch)

let reset bin_path bin_name api state (sources : (Url.t * Stream.t) list) =
  let exec_path = (Filename.concat bin_path bin_name) in
  let ids =
    List.map (fun (_, s) ->
        Stream.ID.to_string s.Stream.id) sources in
  let uris = List.map Fun.(fst %> Url.to_string) sources in
  let args = List.interleave ids uris in
  let exec_opts = Array.of_list (bin_name :: args) in
  (* ignore @@ Lwt_io.printf "Arguments: %s\n" (Array.fold_left (fun acc s -> acc ^ " " ^ s) "" exec_opts); *)
  api.sources := sources;

  Exchange.reset state.socket;

  Option.iter (fun proc -> proc#terminate) state.proc;

  let is_ready = Exchange.Ready.is_ready state.ready_e in
  state.proc <- Some (Lwt_process.open_process_none (exec_path, exec_opts));

  Lwt.on_termination is_ready (Exchange.on_ready state.socket (fun () -> ()));

  (*Lwt.ignore_result (is_ready
                     >|= Exchange.on_ready state.socket (fun () -> ()));*)
  (*settings_init typ send state.options)); *)

  Model.set_streams api.model (List.map snd sources);
  Logs.debug (fun m -> m "(Pipeline) reset [%s]" (Array.fold_left (fun acc x -> acc ^ " " ^ x) "" exec_opts))

let finalize state =
  Exchange.finalize state.socket;
  Option.iter (fun proc -> proc#terminate) state.proc;
  state.proc <- None;
  Logs.debug (fun m -> m "(Pipeline) finalize")
