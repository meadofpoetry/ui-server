open Application_types
open Pipeline_types

module Qoe_backend = Qoe_backend_lwt.Make
                       (Stream.ID)
                       (Netlib.Uri)
                       (Time.Useconds)
(* TODO rename Qoe_...types.Basic.Time to Usec period *)
   

type options =
  { wm : Wm.t Kv_v.rw
  ; structures : Structure.t list Kv_v.rw
  ; settings : unit (* Settings.t Kv_v.rw *)
  }

type notifications =
  { streams : Structure.t list React.signal
  (*; settings : Settings.t React.signal*)
  ; wm : Wm.t React.signal
  ; applied_structs : Structure.t list React.signal
  ; status : Qoe_status.t list React.signal
  ; status_raw : Qoe_status.t React.event
  ; vdata : Qoe_errors.Video_data.t React.event (* TODO to be split by purpose later *)
  ; adata : Qoe_errors.Audio_data.t React.event
  }
  
type state =
  { mutable backend : Qoe_backend.t option
  ; mutable running : unit Lwt.t option
  ; mutable notifs : notifications
  ; mutable sources : (Netlib.Uri.t * Stream.t) list
  ; options : options
  ; model   : Model.t
  ; cleanup : < set_cb : (unit -> unit) -> unit; call : unit >
  }

module Wm_options = Kv_v.RW(Wm)
module Structures_options = Kv_v.RW(Structure.Many)
(*module Settings_options = Kv_v.RW(Settings)*)

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
   (*                     
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
    *)
  (*
let create_notifications (options : options) : (notifications * notify) =  
  (* TODO remove Equal.poly *)
  let streams, streams_push = Util_react.E.create () in
  let wm, wm_push = Util_react.E.create () in
  let applied_structs, applied_push = Util_react.E.create () in
  let status_raw, status_push = Util_react.E.create () in
  let vdata, vdata_push = Util_react.E.create () in
  let adata, adata_push = Util_react.E.create () in
  
  (*
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
   *)
  let streams = Util_react.S.hold ~eq:Structure.equal_many [] streams in
  let wm = Util_react.S.hold ~eq:Wm.equal Wm.default wm in
  let applied_structs = Util_react.S.hold ~eq:Structure.equal_many [] applied_structs in
  
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
  let notif_push = { streams = streams_push
                   ; wm = wm_push
                   ; applied_structs = applied_push
                   ; status = status_push
                   ; vdata = vdata_push
                   ; adata = adata_push
                   }
  in
  (notifs, notif_push)
   *)

let notifs_default = { streams = React.S.const []
                     ; wm = React.S.const Wm.default
                     ; applied_structs = React.S.const []
                     ; status = React.S.const []
                     ; status_raw = React.E.never
                     ; vdata = React.E.never
                     ; adata = React.E.never
                     }

let init_streams stream_options events = failwith "not impl"

let init_wm stream_options events = failwith "not impl"
                  
let create db kv =
  let (>>=?) = Lwt_result.bind in

  Wm_options.create ~default:Wm.default kv ["pipeline";"wm"]
  >>=? fun wm ->

  Structures_options.create ~default:[] kv ["pipeline";"structures"]
  >>=? fun structures ->
(*
  Settings_options.create ~default:Settings.default kv ["pipeline";"settings"]
  >>=? fun settings ->
 *)
  
  let options = { wm; structures; settings = () } in
  let sources = [] in

  Qoe_backend.init_logger ();
  let backend = None in
  let running = None in

  (*let merge v = Structure_conv.match_streams srcs v in*)
  (*
  let (notifs, notify) = create_notifications options in
   *)
  let notifs = notifs_default in
  
  Model.create db notifs.streams notifs.status notifs.vdata notifs.adata
  >>=? fun model ->

  let cleanup =
    let cb = ref (fun () -> ()) in
    object
      method set_cb new_cb = cb := new_cb
      method call = !cb ()
    end
  in
  
  let state =
    { backend; running; notifs; sources; options; model; cleanup }
  in
  Lwt.return_ok (state)
  
let reset state (sources : (Netlib.Uri.t * Stream.t) list) =
  let (>>=) = Lwt.bind in
  let (>>=?) = Lwt_result.bind in
  
  let ids =
    List.map (fun (_, s) ->
        Stream.ID.to_string s.Stream.id) sources in
  let uris = List.map (fun (a,_) -> Netlib.Uri.to_string a) sources in
  let args = Array.of_list @@ List.combine ids uris in

  begin match state.backend with
  | None -> Lwt.return_unit
  | Some backend ->
     match state.running with
     | None -> Lwt.return_unit
     | Some t ->
        Qoe_backend.destroy backend;
        t
  end
  >>= fun () ->

  Qoe_backend.create args
  >>=? fun (backend, events) ->
  
  state.backend <- Some backend;

  let streams = init_streams state.options.structures events.streams in
  let wm = init_wm state.options.wm events.wm in

  state.notifs <- { streams
                  ; wm
                  ; applied_structs = React.S.const [] (* TODO *)
                  ; status = React.S.const []    (* TODO *)
                  ; status_raw = events.status
                  ; vdata = events.vdata
                  ; adata = events.adata
                  };

  state.cleanup#call;

  (* TODO add state updates if settings are avail *)

  Model.set_streams state.model (List.map snd sources);

  state.running <- Some (Qoe_backend.run backend);

  Lwt.return_ok ()

let finalize state =
  let (>>=) = Lwt.bind in
  begin match state.backend with
  | None -> Lwt.return_unit
  | Some backend ->
     match state.running with
     | None -> Lwt.return_unit
     | Some t ->
        Qoe_backend.destroy backend;
        t
  end
  >>= fun () ->

  state.backend <- None;
  state.running <- None;
  state.notifs <- notifs_default;
  state.cleanup#call;
  Logs.debug (fun m -> m "(Pipeline) finalize");
  Lwt.return_unit
