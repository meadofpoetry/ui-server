open Containers
open Lwt.Infix
open Msg_conv
open Message
open Notif
open Qoe_errors
open Interop
open Common

type options =
  { wm : Wm.t Storage.Options.storage
  ; structures : Structure.Structures.t Storage.Options.storage
  ; settings : Settings.t Storage.Options.storage
  }

type state =
  { socket : Exchange.t
  ; ready_e : unit React.event
  ; options : options
  ; srcs : (Url.t * Stream.t) list ref
  ; mutable proc : Lwt_process.process_none option
  }

type channels =
  { wm : Wm.t channel
  ; streams : Structure.t list channel
  ; settings : Settings.t channel
  ; applied_structs : Graph.Applied_structures.t request
  }

type notifs =
  { streams : Structure.t list React.signal
  ; settings : Settings.t React.signal
  ; wm : Wm.t React.signal
  ; applied_structs : Structure.structure list React.signal
  ; status : Qoe_status.t list React.signal
  ; vdata : Video_data.t React.event (* TODO to be split by purpose later *)
  ; adata : Audio_data.t React.event
  }

type api =
  { notifs : notifs
  ; requests : channels
  ; model : Model.t
  }

module Wm_options = Storage.Options.Make(Wm)
module Structures_options = Storage.Options.Make(Structure.Structures)
module Settings_options = Storage.Options.Make(Settings)

module Wm_notif = Notif.Make(Wm)
module Structures_notif = Notif.Make(Structure.Structures)
module Settings_notif = Notif.Make(Settings)
module Applied_structures_notif = Notif.Make(Graph.Applied_structures)
module Video_data_notif = Notif.Make(Video_data)
module Audio_data_notif = Notif.Make(Audio_data)
module Qoe_status_notif = Notif.Make(Qoe_status)

module Wm_msg = Message.Make(Wm)
module Structure_msg = Message.Make(Structure.Structures)
module Settings_msg = Message.Make(Settings)
module Applied_structures_request = Message.Make_request(Graph.Applied_structures)

(*
let settings_init typ send (options : options) =
  let set_chan = Settings_msg.create typ send in
  Lwt.ignore_result @@ (set_chan.set options.settings#get
                        >>= function  Ok ()   -> Lwt_io.printf "Settings resp: fine\n"
                                    | Error r -> Lwt_io.printf "Settings resp: %s\n" r)
 *)
                           
let notification_signal (type a b)
      ~(combine: set:a -> b -> [`Kept of a | `Changed of a | `Updated of a ])
      ~channel
      ~options
      ~(signal:b React.signal) =
  let make_setter merge set value =
    match merge value with
    | `Kept v -> Some v
    | `Updated v -> (options#store v; Some v)
    | `Changed v -> match Lwt_main.run @@ set v with
                    | Ok () -> Some v
                    | Error _e -> None (* TODO add log *)
                    | exception Failure _e -> None
  in                   
  let signal_add_setter signal default setter =
    let signal = React.S.limit ~eq:Pervasives.(=) (fun () -> Lwt_unix.sleep 0.5) signal in
    React.S.fmap ~eq:Pervasives.(=) setter default signal
  in
  let merge   = make_setter (fun (x : b) -> combine ~set:(options#get) x) channel.set in
  let default = options#get in
  signal_add_setter signal default merge

type (_,_) conv = Preserve : ('a,'a) conv
                | Convert :  { into:'a -> 'b; from:'b -> 'a } -> ('a, 'b) conv
  
let msg_channel (type a b)
      ~(convert : (a,b) conv)
      ~update
      ~(channel : a channel) : b channel =
  let open Lwt_result in
  let set' x =
    channel.set x >|= fun () ->
    update x
  in match convert with
     | Preserve  -> ({ channel with set = set' } : b channel)
     | Convert c -> { get = (fun () -> channel.get () >|= c.into);
                      set = (fun x  -> set' @@ c.from x)
                    }

let init_exchange (type a) (typ : a typ) send structures_packer (options : options) =
  let create_table lst =
    let table = Hashtbl.create 10 in
    List.iter (fun (n,f) -> Hashtbl.add table n f) lst;
    table
  in
  let pid_to_status (stream,channel,pid,_) : Qoe_status.t =
    { stream; channel; pid; playing = true }
  in
  let pid_diff post prev =
    let is_in (s,c,p,_) = List.exists (fun (sp,cp,pp,_) ->
                              Stream.ID.equal s sp && c = cp && p = pp)
    in `Diff (object
      method appeared = List.filter (fun x -> not @@ is_in x prev) post
      method disappeared = List.filter (fun x -> not @@ is_in x post) prev
    end)
  in
  let update_status (lst : Qoe_status.t list) event =
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
                         not @@ List.exists (fun (s,c,p,_) ->
                                    Stream.ID.equal s x.stream && c = x.channel && p = x.pid)
                                  o#disappeared) lst
       in (List.map pid_to_status o#appeared) @ new_lst
    | `Status s -> apply_status s lst
  in
  let unwrap f result =
    match result with
    | Ok r -> (f r : unit)
    | Error e -> Logs.err (fun m -> m "(Pipeline) notification parse error %s" e)
  in
  let events, epush = React.E.create () in
  let ready, ready_push = React.E.create () in
  let strm, strm_push = React.E.create () in
  let sets, sets_push = React.E.create () in
  let wm  , wm_push = React.E.create () in
  let applied_structs, graph_push =
    React.S.create ~eq:(Equal.list Structure.equal_structure) [] in
  let vdata, vdata_push = React.E.create () in
  let adata, adata_push = React.E.create () in
  let stat, stat_push = React.E.create () in
  let table =
    create_table
      [ Notif.Ready.create typ (unwrap ready_push)
      ; Wm_notif.create typ (unwrap wm_push)
      ; Structures_notif.create typ (unwrap strm_push)
      ; Settings_notif.create typ (unwrap sets_push)
      ; Applied_structures_notif.create typ (unwrap graph_push)
      ; Video_data_notif.create typ (unwrap vdata_push)
      ; Audio_data_notif.create typ (unwrap adata_push)
      ; Qoe_status_notif.create typ (unwrap stat_push)
      ]
  in
  let dispatch = dispatch typ table in
  React.E.keep @@ React.E.map dispatch events;
  
  let requests = { wm =  msg_channel
                           ~convert:Preserve
                           ~update:(fun x -> wm_push x; options.wm#store x)
                           ~channel:(Wm_msg.create typ send)
                 ; streams = msg_channel
                               ~convert:(Convert { into = structures_packer
                                                 ; from = Structure.Streams.unwrap })
                               ~update:(fun x -> strm_push x; options.structures#store x)
                               ~channel:(Structure_msg.create typ send)
                 ; settings = msg_channel
                                ~convert:Preserve
                                ~update:(fun x -> sets_push x; options.settings#store x)
                                ~channel:(Settings_msg.create typ send)
                 ; applied_structs = Applied_structures_request.create typ send }
  in

  let structures = notification_signal
                     ~combine:Structure.Structures.combine
                     ~channel:(Structure_msg.create typ send)
                     ~options:(options.structures)
                     ~signal:(React.S.l2 ~eq:Pervasives.(=) Pair.make
                                applied_structs
                                (React.S.hold ~eq:Pervasives.(=) options.structures#get strm))
  in

  let wm = notification_signal
             ~combine:Wm.combine
             ~channel:(Wm_msg.create typ send)
             ~options:(options.wm)
             ~signal:(React.S.hold ~eq:Wm.equal options.wm#get wm)
  in

  let settings = notification_signal
                   ~combine:Settings.combine
                   ~channel:(Settings_msg.create typ send)
                   ~options:(options.settings)
                   ~signal:(React.S.hold ~eq:Settings.equal options.settings#get sets)
  in
  
  let streams =
    React.S.map ~eq:(Equal.list Structure.equal_packed) structures_packer structures
  in
  
  (* TODO reimplement *)
  let pids_diff =
    let eq = fun (a, b, c, d) (e, f, g, h) ->
      Stream.ID.equal a e
      && Int.equal b f
      && Int.equal c g
      && Bool.equal d h in
    React.S.diff pid_diff
    @@ React.S.map ~eq:(Equal.list eq)
         (fun x -> Structure.active_pids x) applied_structs in

  let status =
    React.S.fold ~eq:(fun _ _ -> false) update_status []
    @@ React.E.select [pids_diff; React.E.map (fun x -> `Status x) stat] in
  React.E.map_p (fun x ->
      Lwt_io.printf "Signal: %s %d %d (%b)\n"
        (Stream.ID.to_string x.Qoe_status.stream) x.channel x.pid x.playing)
    stat
  |> React.E.keep;
  
  let notifs =
    { streams; wm; settings; applied_structs; adata; vdata; status }
  in
  epush, ready, notifs, requests

let create (type a) (typ : a typ) db_conf config sock_in sock_out =
  let stor    = Storage.Options.Conf.get config in
  let options =
    { wm = Wm_options.create stor.config_dir ["pipeline";"wm"]
    ; structures = Structures_options.create stor.config_dir ["pipeline";"structures"]
    ; settings = Settings_options.create stor.config_dir ["pipeline";"settings"]
    } in
  let srcs = ref [] in
  let proc = None in
  let converter = Msg_conv.get_converter typ in

  let socket = Exchange.create typ converter ~sock_in ~sock_out in
  let send, recv = Exchange.get_send_and_recv socket converter in (* TODO remove converter *)

  let merge v = Structure_conv.match_streams srcs v in

  let epush, ready_e, notifs, requests =
    init_exchange typ send merge options in

  let model = Model.create db_conf notifs.streams notifs.status notifs.vdata notifs.adata in
  let api =
    { notifs; requests; model }
  in
  let state =
    { socket; ready_e; options; srcs; proc }
  in
  api, state, (recv epush), send

let reset typ send bin_path bin_name msg_fmt api state (sources : (Url.t * Stream.t) list) =
  let exec_path = (Filename.concat bin_path bin_name) in
  let msg_fmt = Pipeline_settings.format_to_string msg_fmt in
  let ids =
    List.map (fun (_, s) ->
        Stream.ID.to_string s.Stream.id) sources in
  let uris = List.map Fun.(fst %> Url.to_string) sources in
  let args = List.interleave ids uris in
  let exec_opts = Array.of_list (bin_name :: "-m" :: msg_fmt :: args) in
  (* ignore @@ Lwt_io.printf "Arguments: %s\n" (Array.fold_left (fun acc s -> acc ^ " " ^ s) "" exec_opts); *)
  state.srcs := sources;

  Exchange.reset state.socket;

  Option.iter (fun proc -> proc#terminate) state.proc;

  let is_ready = Notif.is_ready state.ready_e in
  state.proc <- Some (Lwt_process.open_process_none (exec_path, exec_opts));

  Lwt.ignore_result (is_ready
                     >|= Exchange.on_ready state.socket (fun () -> ()));
  (*settings_init typ send state.options)); *)

  Model.set_streams api.model (List.map snd sources);
  Logs.debug (fun m -> m "(Pipeline) reset [%s]" (Array.fold_left (fun acc x -> acc ^ " " ^ x) "" exec_opts))

let finalize state =
  Exchange.finalize state.socket;
  Option.iter (fun proc -> proc#terminate) state.proc;
  state.proc <- None;
  Logs.debug (fun m -> m "(Pipeline) finalize")
