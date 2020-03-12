open Application_types
open Pipeline_types
module Qoe_backend =
  Qoe_backend_lwt.Make (Stream.ID) (Netlib.Uri) (Time.Useconds)
    (Time.Period.Useconds)

(* TODO rename Qoe_...types.Basic.Time to Usec period *)

type options = {
  wm : Wm.t Kv_v.rw;
  structures : Structure.t list Kv_v.rw;
  settings : Settings.t Kv_v.rw;
}

type notifications = {
  streams : Structure.t list React.signal;
  wm : Wm.t React.signal;
  applied_structs : Structure.t list React.signal;
  status : Qoe_status.t list React.signal;
  status_raw : Qoe_status.t React.event;
  vdata : Qoe_errors.Video_data.t React.event;
  (* TODO to be split by purpose later *)
  adata : Qoe_errors.Audio_data.t React.event;
  streams_reset : unit React.signal;
  settings_reset : unit React.signal;
  wm_reset : unit React.signal;
}

type state = {
  mutable backend : Qoe_backend.t option;
  mutable running : unit Lwt.t option;
  mutable notifs : notifications;
  mutable sources : (Netlib.Uri.t * Stream.t) list;
  options : options;
  model : Model.t;
      (* ; cleanup : < set_cb : (unit -> unit) -> unit; call : unit > *)
}

module Wm_options = Kv_v.RW (Wm)
module Structures_options = Kv_v.RW (Structure.Many)
module Settings_options = Kv_v.RW (Settings)

let notifs_default =
  {
    streams = React.S.const [];
    wm = React.S.const Wm.default;
    applied_structs = React.S.const [];
    status = React.S.const [];
    status_raw = React.E.never;
    vdata = React.E.never;
    adata = React.E.never;
    streams_reset = React.S.const ();
    settings_reset = React.S.const ();
    wm_reset = React.S.const ();
  }

(* TODO remove in 4.08 *)
let list_equal eq l r =
  try
    let in_r, not_in_r =
      List.partition (fun el -> List.exists (fun er -> eq el er) r) l
    in
    if not_in_r <> [] then raise_notrace Not_found;
    List.iter
      (fun er ->
        if not @@ List.exists (fun el -> eq el er) in_r then
          raise_notrace Not_found)
      r;
    true
  with Not_found -> false

let limit_inert ~eq t s =
  let open React in
  let tm = ref Lwt.return_unit in
  let event, epush = E.create () in
  let iter =
    E.fmap
      (fun x ->
        if Lwt.is_sleeping !tm then (
          Lwt.cancel !tm;
          tm := Lwt_unix.sleep t;
          Lwt.on_success !tm (fun () -> epush x);
          None )
        else (
          tm := Lwt_unix.sleep t;
          Lwt.on_success !tm (fun () -> epush x);
          None ))
      (S.changes s)
  in
  S.hold ~eq (S.value s) (E.select [ iter; event ])

let notification_mutex = Lwt_mutex.create ()

(* TODO simplify and remove mutex *)
let notification_attach_setter
    ~(combine : set:'a -> 'b -> [< `Kept of 'a | `Changed of 'a ] Lwt.t)
    ~options ~(set : 'a -> (unit, [> ]) result Lwt.t)
    ~(signal : 'b React.signal) =
  let ( >>= ) = Lwt.bind in
  let make_setter merge set value =
    merge value >>= function
    | `Kept _v -> Lwt.return_unit
    | `Changed v ->
        Lwt.try_bind
          (fun () -> set v)
          (function
            | Ok () -> Lwt.return_unit
            | Error _e -> Lwt.return_unit (* TODO add log *))
          (function _ -> Lwt.return_unit)
  in
  let signal_add_setter signal _default setter =
    let signal = limit_inert ~eq:Stdlib.( = ) 2.0 signal in
    Util_react.S.map_s ~eq:Stdlib.( = ) setter signal
  in
  let merge v =
    Lwt_mutex.with_lock notification_mutex (fun () ->
        make_setter
          (fun (x : 'b) -> options#get >>= fun set -> combine ~set x)
          set v)
  in
  options#get >>= fun default -> signal_add_setter signal default merge

let make_streams_with_restore_config ~apply_settings ~get_applied stream_options
    stream_signal =
  let ( >>= ) = Lwt.bind in
  let combine ~set avail =
    get_applied () >>= function
    | Error _ ->
        Lwt.return
          (Structure.Annotated.update_stored ~stored:set ~active:[] ~avail)
    | Ok active ->
        Lwt.return
          (Structure.Annotated.update_stored ~stored:set ~active ~avail)
  in
  notification_attach_setter ~combine ~set:apply_settings
    ~options:stream_options ~signal:stream_signal

let make_wm_with_restore_config ~apply_settings wm_options wm =
  let combine ~set wm =
    Lwt.return @@ Wm.Annotated.update_stored ~stored:set ~active:wm
  in
  notification_attach_setter ~combine ~set:apply_settings ~options:wm_options
    ~signal:wm

(* TODO simplify *)
let merge_status stream_options status_raw =
  let pid_equal (a, b, c) (e, f, g) = Stream.ID.equal a e && b = f && c = g in

  let pid_diff post prev =
    let is_in pid = List.exists (pid_equal pid) in
    `Diff
      (object
         method appeared = List.filter (fun x -> not @@ is_in x prev) post

         method disappeared = List.filter (fun x -> not @@ is_in x post) prev
      end)
  in

  let update_status (lst : Qoe_status.t list) event =
    let pid_to_status (stream, channel, pid) : Qoe_status.t =
      { stream; channel; pid; playing = true }
    in
    let rec apply_status (entry : Qoe_status.t) :
        Qoe_status.t list -> Qoe_status.t list = function
      | [] -> []
      | h :: tl ->
          if
            Stream.ID.equal h.stream entry.stream
            && h.channel = entry.channel
            && h.pid = entry.pid
          then entry :: tl
          else h :: apply_status entry tl
    in
    match event with
    | `Diff o ->
        let new_lst =
          List.filter
            (fun (x : Qoe_status.t) ->
              not
              @@ List.exists
                   (fun (s, c, p) ->
                     Stream.ID.equal s x.stream && c = x.channel && p = x.pid)
                   o#disappeared)
            lst
        in
        List.map pid_to_status o#appeared @ new_lst
    | `Status s -> apply_status s lst
  in

  let pids_diff =
    stream_options#s
    |> React.S.map ~eq:(list_equal pid_equal) (fun x -> Structure.pids x)
    |> React.S.diff pid_diff
  in

  React.E.select [ pids_diff; React.E.map (fun x -> `Status x) status_raw ]
  |> React.S.fold ~eq:(fun _ _ -> false) update_status []

let create db kv =
  let ( >>=? ) = Lwt_result.bind in

  Wm_options.create ~default:Wm.default kv [ "pipeline"; "wm" ] >>=? fun wm ->
  Structures_options.create ~default:[] kv [ "pipeline"; "structures" ]
  >>=? fun structures ->
  Settings_options.create ~default:Settings.default kv
    [ "pipeline"; "settings" ]
  >>=? fun settings ->
  let options = { wm; structures; settings } in
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
  (*
  let cleanup =
    let cb = ref (fun () -> ()) in
    object
      method set_cb new_cb = cb := new_cb
      method call = !cb ()
    end
  in
 *)
  let state = { backend; running; notifs; sources; options; model } in
  Lwt.return_ok state

let backend_lock = Lwt_mutex.create ()

let reset state (sources : (Netlib.Uri.t * Stream.t) list) =
  let ( >>= ) = Lwt.bind in
  let ( >>=? ) = Lwt_result.bind in

  Lwt_mutex.with_lock backend_lock (fun () ->
      let args =
        List.map (fun (uri, s) -> (s.Stream.id, uri)) sources |> Array.of_list
      in

      ( match state.backend with
      | None -> Lwt.return_unit
      | Some backend -> (
          match state.running with
          | None -> Lwt.return_unit
          | Some t ->
              Qoe_backend.destroy backend;
              t ) )
      >>= fun () ->
      Qoe_backend.create args >>=? fun (backend, events) ->
      state.backend <- Some backend;

      let streams =
        Util_react.S.hold ~eq:Structure.equal_many [] events.streams
      in

      let applied_structs =
        Util_react.S.hold ~eq:Structure.equal_many [] events.graph
      in

      let wm = Util_react.S.hold ~eq:Wm.equal Wm.default events.wm in

      (*
  make_streams_with_restore_config
    ~apply_settings:(fun st -> Qoe_backend.Graph.apply_structure backend st)
    ~get_applied:(fun () -> Qoe_backend.Graph.get_structure backend)
    state.options.structures
    streams
  >>= fun streams_reset ->

  make_wm_with_restore_config
    ~apply_settings:(fun wm -> Qoe_backend.Mosaic.apply_layout backend wm)
    state.options.wm
    wm
  >>= fun wm_reset ->
       *)
      let status = merge_status state.options.structures events.status in

      state.notifs <-
        {
          streams;
          wm;
          applied_structs;
          status;
          status_raw = events.status;
          vdata = events.vdata;
          adata = events.adata (*  *);
          settings_reset = React.S.const ();
          streams_reset = React.S.const ();
          wm_reset = React.S.const ();
        };

      (*
  state.cleanup#call;
       *)
      (* TODO add state updates if settings are avail *)
      Model.set_streams state.model (List.map snd sources);

      state.running <- Some (Qoe_backend.run backend);

      state.sources <- sources;

      Qoe_backend.Analysis_settings.apply_settings backend
        (React.S.value state.options.settings#s))

let finalize state =
  let ( >>= ) = Lwt.bind in

  Lwt_mutex.with_lock backend_lock (fun () ->
      ( match state.backend with
      | None -> Lwt.return_unit
      | Some backend -> (
          match state.running with
          | None -> Lwt.return_unit
          | Some t ->
              Qoe_backend.destroy backend;
              t ) )
      >>= fun () ->
      state.backend <- None;
      state.running <- None;
      state.notifs <- notifs_default;
      (* state.cleanup#call; *)
      Logs.debug (fun m -> m "(Pipeline) finalize");
      Lwt.return_unit)
