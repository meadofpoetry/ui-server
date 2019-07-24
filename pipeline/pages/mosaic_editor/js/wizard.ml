open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Pipeline_types

let ( >>= ) = Lwt.( >>= )

type event =
  [ `Streams of Structure.Annotated.t
  | `Layout of Wm.t
  ]

type channel =
  { stream : Stream.ID.t
  ; channel : int
  }

let split_three l =
  List.fold_left (fun (first, second, third) (a, b, c) ->
      a :: first, b :: second, c :: third) ([], [], []) l

(* module Parse_struct = struct
 * 
 *   let stream id (signal : Structure.Annotated.t) =
 *     let packed =
 *       List.find_opt (fun (state, (structure : Structure.Annotated.structure)) ->
 *           Stream.ID.equal structure.id id) signal in
 *     match packed with
 *     | None -> None
 *     | Some (_, structure) ->
 *       Some (Stream.Source.to_string structure.source.info,
 *             Uri.to_string packed.structure.uri, packed)
 * 
 *   let channel (channel_ : int) (structure : Structure.Annotated.structure) =
 *     let channel =
 *       List.find_opt (fun (ch : Structure.channel) -> ch.number = channel_)
 *         structure.channels in
 *     match channel with
 *     | None -> None
 *     | Some channel -> Some (channel.service_name, channel.provider_name, channel)
 * 
 *   let widget pid_ (channel : Structure.channel) =
 *     let pid = List.find_opt (fun (pid : Structure.pid) ->
 *         pid.pid = pid_) channel.pids in
 *     match pid with
 *     | None -> None
 *     | Some pid ->
 *       let stream_type =
 *         Application_types.MPEG_TS.stream_type_to_string
 *           pid.stream_type in
 *       Some (stream_type,
 *             "PID "
 *             ^ (string_of_int pid.pid)
 *             ^ (Printf.sprintf " (0x%04X)" pid.pid))
 * 
 * end *)

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

  type data =
    { widget : string * Wm.widget
    ; service_name : string
    ; provider_name : string
    } [@@deriving yojson]

  (* makes a checkbox with id of domains and typ, and a tree item named by channel*)
  let make_widget (widget : (string * Wm.widget) * channel)
      (channel_struct : Structure.Annotated.channel) =
    let widget, _channel = widget in
    let typ = (snd widget).type_ in
    let default_text () =
      match typ with
      | Video -> "Видео"
      | Audio -> "Аудио" in
    let text, secondary_text =
      match (snd widget).pid with
      | Some _pid -> "", ""
        (* (match Parse_struct.widget pid channel_struct with
         *  | None -> default_text (), Printf.sprintf "PID: %d (0x%04X)" pid pid
         *  | Some s -> s) *)
      | None -> default_text (), "" in
    let checkbox = Checkbox.make () in
    let data =
      { widget
      ; service_name = channel_struct.service_name
      ; provider_name = channel_struct.provider_name
      } in
    let node =
      Treeview.make_node
        ~secondary_text
        ~graphic:checkbox#root
        ~value:(Yojson.Safe.to_string @@ data_to_yojson data)
        text in
    node

  (* makes all the widgets checkboxes with IDs, checkboxes of channels Tree items,
   * and a Tree.t containing all given channels *)
  let make_channels
      (widgets : ((string * Wm.widget) * channel) list)
      (_state, (structure : Structure.Annotated.structure)) =
    let channels = Find.channels widgets in
    List.rev
    @@ List.fold_left (fun acc channel ->
        let channel, stream = channel.channel, channel.stream in
        let channel_struct =
          List.find_opt (fun (_, (ch : Structure.Annotated.channel)) ->
              channel = ch.number) structure.channels in
        match channel_struct with
        | None -> acc
        | Some (_, channel_struct) ->
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
      (_structure : Structure.Annotated.t) =
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
      List.fold_left (fun acc (_stream, _wds) ->
          acc
          (* match Parse_struct.stream stream structure with
           * | None -> acc
           * | Some (text, secondary_text, packed) ->
           *   let channels = make_channels wds packed in
           *   let checkbox = Checkbox.make () in
           *   let stream_node =
           *     Treeview.make_node
           *       ~secondary_text
           *       ~graphic:checkbox#root
           *       ~children:channels
           *       ~value:(Stream.ID.to_string stream)
           *       text in
           *   stream_node :: acc *))
        [] streams_of_widgets in
    Treeview.make ~dense:true ~two_line:true nodes

end

let to_content (streams : Structure.Annotated.t)
    (wm : Wm.Annotated.t) =
  let widgets = Utils.List.filter_map (fun (name, (widget : Wm.widget)) ->
      match (widget.domain : Wm.domain) with
      | (Chan {stream; channel} : Wm.domain) ->
        Some ((name, widget), ({ stream; channel } : channel))
      | (Nihil : Wm.domain) -> None) wm.widgets in
  Branches.make_streams widgets streams

let layout_of_widgets ~resolution (data : Branches.data list)
  : ((string * Wm.container) list) =
  (* TODO implement *)
  ignore resolution;
  ignore data;
  []

class t ~resolution ~treeview (elt : Dom_html.element Js.t) () =
  object
    inherit Dialog.t elt ()

    val mutable resolution = resolution
    val mutable _treeview : Treeview.t = treeview

    method value =
      let data =
        Utils.List.filter_map (fun x ->
            match _treeview#node_value x with
            | None -> None
            | Some json ->
              try
                match Branches.data_of_yojson @@ Yojson.Safe.from_string json with
                | Error _ -> None
                | Ok x -> Some x
              with _ -> None)
        @@ _treeview#selected_leafs in
      layout_of_widgets ~resolution data

    (* TODO implement *)
    method notify : event -> unit = function
      | `Streams _streams -> ()
      | `Layout layout -> resolution <- layout.resolution
  end

let make
    (streams : Structure.Annotated.t)
    (wm : Wm.Annotated.t) =
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
  new t
    ~resolution:wm.resolution
    ~treeview:content
    elt ()
