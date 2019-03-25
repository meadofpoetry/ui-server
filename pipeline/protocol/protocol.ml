open Application_types
open Pipeline_types
open Qoe_errors
   
type options =
  { wm : Wm.t Kv_v.rw
  ; structures : Structure.t list Kv_v.rw
  ; settings : Settings.t Kv_v.rw
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
  ; status_raw : Qoe_status.t React.event
  ; vdata : Video_data.t React.event (* TODO to be split by purpose later *)
  ; adata : Audio_data.t React.event
  }

type api =
  { notifs  : notifs
  ; options : options
  ; sources : (Netlib.Uri.t * Stream.t) list ref
  ; channel : Message.chan
  ; model   : Model.t
  }

module Wm_options = Kv_v.RW(Wm)
module Structures_options = Kv_v.RW(Structure.Many)
module Settings_options = Kv_v.RW(Settings)

(*
let settings_init typ send (options : options) =
  let set_chan = Settings_msg.create typ send in
  Lwt.ignore_result @@ (set_chan.set options.settings#get
                        >>= function  Ok ()   -> Lwt_io.printf "Settings resp: fine\n"
                                    | Error r -> Lwt_io.printf "Settings resp: %s\n" r)
 *)

(* TODO remove in 4.08 *)
let list_equal eq l r =
  try
    let in_r, not_in_r = List.partition
                           (fun el -> List.exists (fun er -> eq el er) r) l in
    if not_in_r <> [] then raise_notrace Not_found;
    List.iter (fun er -> if not @@ List.exists (fun el -> eq el er) in_r
                         then raise_notrace Not_found) r;
    true
  with Not_found -> false
                        
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
  && b = f
  && c = g

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
      ~(set : 'a -> (unit, string) result Lwt.t)
      ~(signal:'b React.signal) =
  let (>>=) = Lwt.bind in
  let make_setter merge set value =
    merge value >>= function
    | `Kept v -> Lwt.return_some v
    | `Updated v -> (options#set v >>= fun () -> Lwt.return_some v)
    | `Changed v -> Lwt.try_bind (fun () -> set v)
                      (function Ok () -> Lwt.return_some v
                              | Error _e -> Lwt.return_none (* TODO add log *))
                      (function _ -> Lwt.return_none)
  in                   
  let signal_add_setter signal default setter =
    let signal = limit_inert ~eq:Pervasives.(=) 2.0 signal in
    Util_react.S.fmap_s ~eq:Pervasives.(=) setter default signal
  in
  let merge v = Lwt_mutex.with_lock notification_mutex
                  (fun () -> make_setter (fun (x : 'b) ->
                                 options#get >>= fun set ->
                                 Lwt.return @@ combine ~set x) set v)
  in
  options#get
  >>= fun default ->
  signal_add_setter signal default merge
  
let create_notifications (options : options) channel =
  let open Notif in
  let (>>=) = Lwt.bind in
  
  let ready  = add_event ~name:Exchange.Ready.name Exchange.Ready.ready_of_yojson in
  (* TODO remove Equal.poly *)
  let status_raw = add_event ~name:"stream_lost" Qoe_status.of_yojson in
  let vdata  = add_event ~name:"video_data" Qoe_errors.Video_data.of_yojson in
  let adata  = add_event ~name:"audio_data" Qoe_errors.Audio_data.of_yojson in
  let wm     = add_signal ~name:"wm" ~eq:Wm.equal ~init:Wm.default Wm.of_yojson in
  let streams = add_signal ~name:"stream_parser" ~eq:(fun _ _ -> false) ~init:[]
                  Structure.Many.of_yojson in

  let applied_structs = add_signal ~name:"applied_streams" ~eq:(fun _ _ -> false) ~init:[]
                          Structure.Many.of_yojson in

  notification_attach_setter
    ~combine:Structure.combine
    ~set:(Message.Protocol.graph_apply_structure channel)
    ~options:(options.structures)
    ~signal:(React.S.l2 ~eq:(fun _ _ -> false) (fun a b -> (a,b))
               applied_structs
               streams)
  >>= fun applied_structs ->

  notification_attach_setter
    ~combine:Wm.combine
    ~set:(Message.Protocol.wm_apply_layout channel)
    ~options:(options.wm)
    ~signal:wm
  >>= fun wm ->

  let pids_diff =
    applied_structs
    |> React.S.map ~eq:(list_equal pid_equal) (fun x -> Structure.pids x)
    |> React.S.diff pid_diff
  in
  
  let status =
    React.E.select [pids_diff; React.E.map (fun x -> `Status x) status_raw]
    |> React.S.fold ~eq:(fun _ _ -> false) update_status []
  in
  
  let notifs = { streams; applied_structs; wm; vdata; adata; status; status_raw } in 
  Lwt.return (ready, notifs)
  
let create db kv sock_in sock_out =
  let (>>=?) = Lwt_result.bind in
  let (>>=) = Lwt.bind in

  Wm_options.create ~default:Wm.default kv ["pipeline";"wm"]
  >>=? fun wm ->

  Structures_options.create ~default:[] kv ["pipeline";"structures"]
  >>=? fun structures ->

  Settings_options.create ~default:Settings.default kv ["pipeline";"settings"]
  >>=? fun settings ->
  
  let options = { wm; structures; settings } in
  let sources = ref [] in
  let proc    = None in

  let socket  = Exchange.create ~sock_in ~sock_out in
  let recv    = Exchange.get_recv socket in
  let channel = Exchange.get_chan socket in

  (*let merge v = Structure_conv.match_streams srcs v in*)

  create_notifications options channel
  >>= fun (ready_e, notifs) ->
  
  Model.create db notifs.streams notifs.status notifs.vdata notifs.adata
  >>=? fun model ->
  
  let api =
    { notifs; options; sources; channel; model }
  in
  let state =
    { socket; ready_e; proc }
  in
  Lwt.return_ok (api, state, (recv Notif.dispatch))

let rec interleave l r =
  match l, r with
  | [], r' -> r'
  | l', [] -> l'
  | x::l', y::r' ->
     x::y::(interleave l' r')
  
let reset bin_path bin_name api state (sources : (Netlib.Uri.t * Stream.t) list) =
  let exec_path = (Filename.concat bin_path bin_name) in
  let ids =
    List.map (fun (_, s) ->
        Stream.ID.to_string s.Stream.id) sources in
  let uris = List.map (fun (a,_) -> Netlib.Uri.to_string a) sources in
  let args = interleave ids uris in
  let exec_opts = Array.of_list (bin_name :: args) in
  (* ignore @@ Lwt_io.printf "Arguments: %s\n" (Array.fold_left (fun acc s -> acc ^ " " ^ s) "" exec_opts); *)
  api.sources := sources;

  Exchange.reset state.socket;

  begin match state.proc with
  | None -> ()
  | Some proc -> proc#terminate
  end;

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
  begin match state.proc with
  | None -> ()
  | Some proc -> proc#terminate
  end;
  state.proc <- None;
  Logs.debug (fun m -> m "(Pipeline) finalize")
