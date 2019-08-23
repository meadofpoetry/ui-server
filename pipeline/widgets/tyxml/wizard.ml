open Components_tyxml
open Application_types
open Pipeline_types

module CSS = struct
  let root = "pipeline-wizard"

  let hint = BEM.add_element root "hint"
end

type channel =
  { stream : Stream.ID.t
  ; channel : int
  }

type data =
  { widget : string * Wm.widget
  ; service_name : string
  ; provider_name : string
  } [@@deriving yojson]

module Parse_struct = struct
  open Structure.Annotated

  let widget_typ_to_string = function
    | Wm.Video -> "Video"
    | Audio -> "Audio"

  let stream sid (signal : t) =
    match List.find_opt (fun (_, {id; _ }) -> Stream.ID.equal id sid) signal with
    | None -> None
    | Some ((_, structure) as s) -> Some (Stream.ID.to_string structure.id, s)

  let widget typ pid_ ({ pids; _ } : channel) =
    match pid_ with
    | None -> widget_typ_to_string typ
    | Some pid_ ->
      let prefix = Printf.sprintf "PID %d (0x%04X)" pid_ pid_ in
      let pid = List.find_opt (fun (_, (pid : Structure.pid)) ->
          pid.pid = pid_) pids in
      let stream_type = match pid with
        | Some x -> MPEG_TS.stream_type_to_string (snd x).stream_type
        | None -> widget_typ_to_string typ in
      Printf.sprintf "%s, %s" prefix stream_type
end

let find_channels widgets =
  List.sort (fun a b -> compare a.channel b.channel)
  @@ List.fold_left (fun acc (widget : ((string * Wm.widget) * channel)) ->
      if List.exists (fun ch -> (snd widget).channel = ch.channel) acc
      then acc
      else (snd widget) :: acc)
    [] widgets

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct

  module Icon = Icon.Make(Xml)(Svg)(Html)
  module Checkbox = Checkbox.Make(Xml)(Svg)(Html)
  module Treeview = Treeview.Make(Xml)(Svg)(Html)
  module Dialog = Dialog.Make(Xml)(Svg)(Html)
  module Placeholder = Components_lab_tyxml.Placeholder.Make(Xml)(Svg)(Html)

  let make_widget_node
      (channel_struct : Structure.Annotated.channel)
      (widget : (string * Wm.widget) * channel)=
    let widget, _channel = widget in
    let typ = (snd widget).type_ in
    let text = Parse_struct.widget typ (snd widget).pid channel_struct in
    let checkbox = Checkbox.create () in
    let data =
      { widget
      ; service_name = channel_struct.service_name
      ; provider_name = channel_struct.provider_name
      } in
    let node =
      Treeview.create_node
        ~graphic:(Html.Unsafe.coerce_elt checkbox)
        ~value:(Yojson.Safe.to_string @@ data_to_yojson data)
        text in
    node

  let make_channel_nodes
      (widgets : ((string * Wm.widget) * channel) list)
      (_state, (structure : Structure.Annotated.structure)) =
    let channels = find_channels widgets in
    List.rev
    @@ List.fold_left (fun acc channel ->
        let channel, stream = channel.channel, channel.stream in
        let channel_struct =
          List.find_opt (fun (_, (ch : Structure.Annotated.channel)) ->
              channel = ch.number) structure.channels in
        match channel_struct with
        | None -> acc
        | Some (_, channel_struct) ->
          let text = channel_struct.service_name in
          let widgets =
            List.sort (fun ((_, (a : Wm.widget)), _) ((_, b), _) ->
                compare a.type_ b.type_)
            @@ List.filter (fun (_, (ch : channel)) ->
                Stream.ID.equal ch.stream stream
                && channel = ch.channel) widgets in
          let children = List.map (make_widget_node channel_struct) widgets in
          let checkbox = Checkbox.create () in
          let node =
            Treeview.create_node
              ~value:text
              ~graphic:(Html.Unsafe.coerce_elt checkbox)
              ~children
              text in
          node :: acc)
      [] channels

  let make_stream_nodes (widgets : ((string * Wm.widget) * channel) list)
      (structure : Structure.Annotated.t) =
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
          | Some (text, packed) ->
            let channels = make_channel_nodes wds packed in
            let checkbox = Checkbox.create () in
            let stream_node =
              Treeview.create_node
                ~graphic:(Html.Unsafe.coerce_elt checkbox)
                ~children:channels
                ~value:(Stream.ID.to_string stream)
                text in
            stream_node :: acc)
        [] streams_of_widgets in
    Treeview.create ~dense:true nodes

  let make_placeholder ?(classes = []) ?(attrs = []) () =
    ignore (classes, attrs);
    ()

  let make_treeview (streams : Structure.Annotated.t)
      (wm : Wm.Annotated.t) =
    let widgets = List.filter_map (fun (name, (widget : Wm.widget)) ->
        match (widget.domain : Wm.domain) with
        | (Chan {stream; channel} : Wm.domain) ->
          Some ((name, widget), ({ stream; channel } : channel))
        | (Nihil : Wm.domain) -> None) wm.widgets in
    make_stream_nodes widgets streams

  let make_empty_placeholder ?classes ?attrs () =
    Placeholder.make_simple ?classes ?attrs
      Icon.SVG.(create [create_path Svg_icons.information ()] ())
      "Нет доступных виджетов"

  let make ?(classes = []) ?(attrs = []) ~treeview () =
    let classes = CSS.root :: classes in
    let title = Dialog.create_title_simple
        ~title:"Автоматическая расстановка виджетов" (* TODO better title *)
        () in
    let content =
      Html.div [ treeview
               ; make_empty_placeholder ()
               ] in
    let actions =
      Dialog.create_actions
        ~actions:[ Dialog.create_action ~action:Close ~label:"Отмена" ()
                 ; Dialog.create_action ~action:Accept ~label:"Применить" ()
                 ]
        () in
    let surface = Dialog.create_surface ~title ~content ~actions () in
    Dialog.create
      ~classes
      ~attrs
      ~scrim:Dialog.(create_scrim ())
      ~container:Dialog.(create_container ~surface ())
      ()
end
