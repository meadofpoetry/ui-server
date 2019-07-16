open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Pipeline_types

let ( >>= ) = Lwt.( >>= )

type event =
  [ `Streams of Structure.packed list
  | `Layout of Wm.t
  ]

type channel =
  { stream : Stream.ID.t
  ; channel : int
  }

let split_three l =
  List.fold_left (fun (first, second, third) (a, b, c) ->
      a :: first, b :: second, c :: third) ([], [], []) l

module Parse_struct = struct

  let stream id (signal : Structure.packed list) =
    let packed =
      List.find_opt (fun (x : Structure.packed) ->
          Stream.ID.equal x.structure.id id) signal in
    match packed with
    | None -> None
    | Some packed ->
      Some (Stream.Source.to_string packed.source.source.info,
            Uri.to_string packed.structure.uri, packed)

  let channel (channel_ : int) (packed : Structure.packed) =
    let channel =
      List.find_opt (fun (ch : Structure.channel) -> ch.number = channel_)
        packed.structure.channels in
    match channel with
    | None -> None
    | Some channel -> Some (channel.service_name, channel.provider_name, channel)

  let widget pid_ (channel : Structure.channel) =
    let pid = List.find_opt (fun (pid : Structure.pid) ->
        pid.pid = pid_) channel.pids in
    match pid with
    | None -> None
    | Some pid -> Some ("PID "
                        ^ (string_of_int pid.pid)
                        ^ (Printf.sprintf " (0x%04X)" pid.pid))

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
      | Some pid ->
        (match Parse_struct.widget pid channel_struct with
         | None -> Printf.sprintf "PID: %d (0x%04X)" pid pid
         | Some s -> s)
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
    List.rev
    @@ List.fold_left (fun acc channel ->
        let channel, stream = channel.channel, channel.stream in
        let channel_struct =
          List.find_opt (fun (ch : Structure.channel) ->
              channel = ch.number) packed.structure.channels in
        match channel_struct with
        | None -> acc
        | Some channel_struct ->
          let text, secondary_text =
            channel_struct.service_name, string_of_int channel in
          let widgets =
            List.filter (fun (_, (ch : channel)) ->
                Stream.ID.equal ch.stream stream
                && channel = ch.channel) widgets in
          let children = List.map (fun widget ->
              make_widget widget channel_struct) widgets in
          let checkbox = Checkbox.make () in
          let node =
            Treeview.make_node
              ~value:text
              ~graphic:checkbox#root
              ~secondary_text
              ~children
              text in
          node :: acc)
      [] channels

  (* makes all the widget checkboxes with IDs, and a Tree.t containing all streams *)
  let make_streams (widgets : ((string * Wm.widget) * channel) list)
      (structure : Structure.packed list) =
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
          match Parse_struct.stream stream structure with
          | None -> acc
          | Some (text, secondary_text, packed) ->
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

let layout_of_widgets ~resolution (widgets : (string * Wm.widget) list)
  : ((string * Wm.container) list) =
  (* TODO implement *)
  []

let widgets_of_treeview
    widgets
    (tree : Treeview.t) =
  let wds =
    Utils.List.filter_map tree#node_value
    @@ tree#selected_leafs in
  List.fold_left (fun acc channel ->
      let typ =
        match String.sub channel (String.length channel - 5) 5 with
        | "Video" -> Wm.Video
        | "Audio" -> Audio
        | _ -> failwith "Wrong widget type!" in
      let channel =
        String.sub channel 0 (String.length channel - 6)
        |> int_of_string in
      match List.filter (fun ((wdg : (string * Wm.widget)), (ch : channel)) ->
          channel = ch.channel
          && Wm.widget_type_equal (snd wdg).type_ typ) widgets with
      | [] -> acc
      | l -> l @ acc) [] wds

let to_content (streams : Structure.packed list) (wm : Wm.t) =
  let widgets = Utils.List.filter_map (fun (name, (widget : Wm.widget)) ->
      match (widget.domain : Wm.domain) with
      | (Chan {stream; channel} : Wm.domain) ->
        Some ((name, widget), ({ stream; channel } : channel))
      | (Nihil : Wm.domain) -> None) wm.widgets in
  Branches.make_streams widgets streams

class t (elt : Dom_html.element Js.t) () =
  object
    inherit Dialog.t elt ()

    method notify : event -> unit = function
      | _ -> ()
  end

let make (streams : Structure.packed list) (wm : Wm.t) =
  let content = to_content streams wm in
  let actions =
    List.map Tyxml_js.Of_dom.of_button
      [ Dialog.make_action ~action:Close ~label:"Отмена" ()
      ; Dialog.make_action ~action:Accept ~label:"Применить" () ] in
  let surface = Dialog.Markup.(
      create_surface
        ~title:(create_title_simple ~title:"Выберите виджеты" ())
        ~content:(create_content ~content:[content#markup] ())
        ~actions:(create_actions ~actions ())
        ()) in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.(
        create
          ~scrim:(create_scrim ())
          ~container:(create_container ~surface ())
          ()) in
  new t elt ()
