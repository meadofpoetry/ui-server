open Containers
open Common
open Components
open Board_types.Streams.TS
open Lwt_result.Infix

module Set = CCSet.Make(Int32)

let ts_found ts_id =
  Printf.sprintf "Обнаружен поток. TS ID: %d" ts_id

let ts_lost ts_id =
  Printf.sprintf "Пропадание потока. TS ID: %d" ts_id

let to_add_event (s:(Stream.id * structure) list React.signal) =
  let open React in
  let open Stream in
  let eq  = equal_id in
  let set = ref (Set.of_list
                 @@ List.map Fun.(id_to_int32 % fst)
                 @@ React.S.value s) in
  S.diff (fun n o ->
      let found = List.filter (fun (x,_) ->
                      not @@ List.Assoc.mem ~eq x o) n in
      let add   = List.filter (fun (x,_) ->
                      not @@ Set.mem (id_to_int32 x) !set) found in
      List.iter (fun (id,_) -> set := Set.add (id_to_int32 id) !set) found;
      add) s

let make_structs
      (state:   Topology.state React.signal)
      (streams: Stream.t list React.signal)
      (signal:  (Stream.id * structure) list React.signal)
      control =
  let to_add = to_add_event signal in
  let snackbar = new Snackbar.t ~message:"" () in
  let box = new Vbox.t ~widgets:[] () in
  let add ?(notify=false) (id,structure) =
    let heading_details = new Typography.Text.t ~text:"" () in
    let details = new Typography.Text.t
                    ~text:(Printf.sprintf "TS ID: %d" structure.general.ts_id)
                    () in
    let s       = React.S.map (List.Assoc.get ~eq:Stream.equal_id id) signal in
    let stream  =
      React.S.map (fun streams ->
          List.find_opt (fun (stream:Stream.t) ->
              match stream.id with
              | `Ts x -> Stream.equal_id id x
              | _     -> false) streams)
        streams in
    let i = Widget_structure.make
              ~config:{ stream = id }
              ~state
              ~init:(Some structure)
              ~event:(React.S.changes s)
              ~stream control () in
    let p = new Expansion_panel.t
              ~title:(Printf.sprintf "MPEG-2 TS")
              ~heading_details:[ heading_details ]
              ~details:[ details ]
              ~content:[i] () in
    let panel_class = "qos-niit-structure-panel" in
    let lost_class  = Markup.CSS.add_modifier panel_class "lost" in
    let () = p#add_class panel_class in
    let _e = (* FIXME keep *)
      React.S.diff (fun n o ->
          match o, n with
          | Some s, None ->
             p#add_class lost_class;
             let message = ts_lost s.general.ts_id in
             snackbar#set_message message;
             snackbar#show ()
          | None, Some s ->
             p#remove_class lost_class;
             let message = ts_found s.general.ts_id in
             snackbar#set_message message;
             snackbar#show ()
          | _ -> ()) s in
    let _s = (* FIXME keep *)
      React.S.map (function
          | Some (s:Stream.t) ->
             Option.iter heading_details#set_text s.description
          | None -> ()) stream in
    if notify
    then (snackbar#set_message @@ ts_found structure.general.ts_id;
          snackbar#show ());
    Dom.appendChild box#root p#root in
  (* FIXME keep *)
  let _e = React.E.map (fun l -> List.iter (add ~notify:true) l) to_add in
  List.iter add @@ React.S.value signal;
  Dom.appendChild Dom_html.document##.body snackbar#root;
  box#widget

let page control () =
  let factory = new Widget_factory.t control () in
  (factory#structs
   >>= fun structs -> factory#state
   >>= fun state   -> factory#streams
   >|= fun streams -> make_structs state streams structs control)
  |> Ui_templates.Loader.create_widget_loader
  |> (fun w -> w#set_on_destroy (Some factory#destroy); w)
