open Js_of_ocaml
open Components
open Application_types
open Pipeline_types

let ( >>= ) = Lwt.( >>= )

type channel =
  { stream : Stream.ID.t
  ; channel : int
  }

let split_three l =
  List.fold_left (fun (first, second, third) (a, b, c) ->
      a :: first, b :: second, c :: third) ([], [], []) l

module Parse_struct = struct

  let stream id (signal : Structure.packed list React.signal) =
    let packed =
      List.find (fun (x : Structure.packed) ->
          Stream.ID.equal x.structure.id id) (React.S.value signal) in
    Stream.Source.to_string packed.source.source.info,
    Uri.to_string packed.structure.uri, packed

  let channel (channel_ : int) (packed : Structure.packed) =
    let channel =
      List.find (fun (ch : Structure.channel) -> ch.number = channel_)
        packed.structure.channels in
    channel.service_name, channel.provider_name, channel

  let widget pid_ (channel : Structure.channel) =
    let pid = List.find (fun (pid : Structure.pid) -> pid.pid = pid_) channel.pids in
    "PID " ^ (string_of_int pid.pid) ^ (Printf.sprintf " (0x%04X)" pid.pid)

end

module Find = struct

  let channels (widgets : ((string * Wm.widget) * channel) list) =
    List.rev
    @@ List.fold_left (fun acc (widget : ((string * Wm.widget) * channel)) ->
        if List.exists (fun ch -> (snd widget).channel = ch.channel) acc
        then acc
        else (snd widget) :: acc)
      [] widgets

  let widget ~(widgets : ((string * Wm.widget) * channel) list) ~(typ : Wm.widget_type) ch =
    List.find_opt (fun ((_, (widget : Wm.widget)), wdg_channel) ->
        Stream.ID.equal ch.stream wdg_channel.stream
        && ch.channel = wdg_channel.channel
        && Wm.widget_type_equal widget.type_ typ) widgets

end

module Branches = struct

  (* makes a checkbox with id of domains and typ, and a tree item named by channel*)
  let make_widget (widget : (string * Wm.widget) * channel) channel_struct =
    let widget, channel = widget in
    let typ = (snd widget).type_ in
    let text =
      match typ with
      | Video -> "Виджет видео"
      | Audio -> "Виджет аудио" in
    let secondary_text =
      match (snd widget).pid with
      | Some pid -> Parse_struct.widget pid channel_struct
      | None -> "" in
    let checkbox = Checkbox.make () in
    let widget_typ =
      match typ with
      | Video -> "Video"
      | Audio -> "Audio" in
    Treeview.make_node
      ~secondary_text
      ~graphic:checkbox#root
      ~value:(string_of_int channel.channel ^ "|" ^ widget_typ)
      text

  (* makes all the widgets checkboxes with IDs, checkboxes of channels Tree items,
   * and a Tree.t containing all given channels *)
  let make_channels
      (widgets : ((string * Wm.widget) * channel) list)
      (packed : Structure.packed) =
    let channels = Find.channels widgets in
    List.map (fun channel ->
        let channel, stream = channel.channel, channel.stream in
        let channel_struct =
          List.find (fun (ch : Structure.channel) ->
              channel = ch.number) packed.structure.channels in
        let text, secondary_text =
          channel_struct.service_name, string_of_int channel in
        let widgets =
          List.filter (fun (_, (ch : channel)) ->
              Stream.ID.equal ch.stream stream
              && channel = ch.channel) widgets in
        let children = List.map (fun widget ->
            make_widget widget channel_struct) widgets in
        let checkbox = Checkbox.make () in
        Treeview.make_node
          ~value:text
          ~graphic:checkbox#root
          ~secondary_text
          ~children
          text) channels

  (* makes all the widget checkboxes with IDs, and a Tree.t containing all streams *)
  let make_streams (widgets : ((string * Wm.widget) * channel) list) signal =
    let streams =
      List.fold_left (fun acc (x : (string * Wm.widget) * channel) ->
          let channel = snd x in
          if List.exists (fun stream ->
              Stream.ID.equal channel.stream stream) acc then
            acc
          else
            channel.stream :: acc) [] widgets in
    let streams_of_widgets =
      List.map (fun stream ->
          let wds =
            List.filter (fun (x : (string * Wm.widget) * channel) ->
                let wdg_stream = (snd x).stream in
                Stream.ID.equal wdg_stream stream) widgets in
          stream, wds) streams in
    let nodes =
      List.fold_left (fun acc (stream, wds) ->
          let text, secondary_text, packed =
            Parse_struct.stream stream signal in
          let channels = make_channels wds packed in
          let checkbox = Checkbox.make () in
          let stream_node =
            Treeview.make_node
              ~secondary_text
              ~graphic:checkbox#root
              ~children:channels
              ~value:(Stream.ID.to_string stream)
              text in
          stream_node :: acc)
        [] streams_of_widgets in
    Treeview.make ~dense:true ~two_line:true nodes

end

let layout_of_widgets (widgets : (string * Wm.widget) list)
  : ((string * Wm.container) list) =
  (* TODO implement *)
  []

(* makes a dialog which shows a tree of available
 * streams
 *    |_ channels
 *        |_ widgets,
 * all with checkboxes.
 * it returns dialog, react event and a fun showing dialog *)

let to_content socket (wm : Wm.t) =
  let open Wm in
  let open Pipeline_http_js in
  let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f) in
  Http_structure.get_streams_applied_with_source ()
  >>= fun init -> Http_structure.Event.get_streams_applied_with_source socket
  >>= fun (id, event) ->
  let s, set_s = React.S.create init in
  let _e = React.E.map set_s event in
  let widgets = Utils.List.filter_map (fun (name, (widget : widget)) ->
      match (widget.domain : domain) with
      | (Chan {stream; channel} : domain) ->
        Some ((name, widget), ({ stream; channel } : channel))
      | (Nihil : domain) -> None) wm.widgets in
  let tree = Branches.make_streams widgets s in
  let box = Box.make ~dir:`Column [tree#widget] in
  box#set_on_destroy (fun () ->
      React.E.stop ~strong:true _e;
      React.S.stop ~strong:true s;
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket id));
  let set = fun () ->
    failwith "FIXME Not impelemented" in
  Lwt.return_ok (box, set)

let to_dialog socket (wm : Wm.t) =
  let thread = to_content socket wm in
  let content =
    Ui_templates.Loader.create_widget_loader
    @@ Lwt_result.map fst thread in
  let dialog = Dialog.make
      ~title:(Dialog.Markup.create_title_simple ~title:"Выберите виджеты" ())
      ~content:(Dialog.Markup.create_content ~content:[content#markup] ())
      ~actions:[ Dialog.Markup.create_action ~action:Close ~label:"Отмена" ()
               ; Dialog.Markup.create_action ~action:Accept ~label:"Применить" () ]
      () in
  let show () =
    let open Lwt.Infix in
    dialog#open_await ()
    >>= function
    | Custom _ | Close | Destroy -> Lwt.return_unit
    | Accept ->
      thread
      >>= function
      | Ok (_, set) -> Lwt.return @@ set ()
      | Error e -> Lwt.return @@ print_endline @@ "Wm_wizard error!: " ^ e in
  dialog, show
