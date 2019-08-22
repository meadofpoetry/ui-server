open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Pipeline_types

let ( >>= ) = Lwt.bind

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

module Parse_struct = struct
  let stream id (signal : Structure.Annotated.t) =
    let packed =
      List.find_opt (fun (_state, (structure : Structure.Annotated.structure)) ->
          Stream.ID.equal structure.id id) signal in
    match packed with
    | None -> None
    | Some ((_, structure) as s) ->
      Some (Stream.ID.to_string structure.id, s)

  let widget typ pid_ ({ pids; _ } : Structure.Annotated.channel) =
    let widget_typ_to_string = function
      | Wm.Video -> "Video"
      | Audio -> "Audio" in
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

module Find = struct
  let channels (widgets : ((string * Wm.widget) * channel) list) =
    List.sort (fun a b -> compare a.channel b.channel)
    @@ List.fold_left (fun acc (widget : ((string * Wm.widget) * channel)) ->
        if List.exists (fun ch -> (snd widget).channel = ch.channel) acc
        then acc
        else (snd widget) :: acc)
      [] widgets
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
    let text = Parse_struct.widget typ (snd widget).pid channel_struct in
    let checkbox = Checkbox.make () in
    let data =
      { widget
      ; service_name = channel_struct.service_name
      ; provider_name = channel_struct.provider_name
      } in
    let node =
      Treeview.make_node
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
          let text = channel_struct.service_name in
          let widgets =
            List.sort (fun ((_, (a : Wm.widget)), _) ((_, b), _) ->
                compare a.type_ b.type_)
            @@ List.filter (fun (_, (ch : channel)) ->
                Stream.ID.equal ch.stream stream
                && channel = ch.channel) widgets in
          let children = List.map (fun widget ->
              make_widget widget channel_struct) widgets in
          let checkbox = Checkbox.make () in
          let node =
            Treeview.make_node
              ~value:text
              ~graphic:checkbox#root
              ~children
              text in
          node :: acc)
      [] channels

  (* makes all the widget checkboxes with IDs, and a Tree.t containing all streams *)
  let make_streams (widgets : ((string * Wm.widget) * channel) list)
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
            let channels = make_channels wds packed in
            let checkbox = Checkbox.make () in
            let stream_node =
              Treeview.make_node
                ~graphic:checkbox#root
                ~children:channels
                ~value:(Stream.ID.to_string stream)
                text in
            stream_node :: acc)
        [] streams_of_widgets in
    Treeview.make ~dense:true nodes

end

let to_content (streams : Structure.Annotated.t)
    (wm : Wm.Annotated.t) =
  let widgets = List.filter_map (fun (name, (widget : Wm.widget)) ->
      match (widget.domain : Wm.domain) with
      | (Chan {stream; channel} : Wm.domain) ->
        Some ((name, widget), ({ stream; channel } : channel))
      | (Nihil : Wm.domain) -> None) wm.widgets in
  Branches.make_streams widgets streams

let compare_domain a b = match a, b with
  | Wm.Nihil, Wm.Nihil -> 0
  | Nihil, Chan _ -> -1
  | Chan _, Nihil -> 1
  | Chan a, Chan b ->
    match Stream.ID.compare a.stream b.stream with
    | 0 -> compare a.channel b.channel
    | x -> x

module type S = sig
  type t
  val container_title : t -> string
  val set_position : Wm.position -> t -> t
  val to_widget : t -> string * Wm.widget
end

module Pair = struct
  type t = int * int
  let compare a b = Int.neg @@ compare a b
end

module Widget_type = struct
  type t = Wm.widget_type
  let compare = compare
end

module Domain = struct
  type t = Wm.domain
  let compare = compare_domain
end

module Make(S : S) = struct

  module Aspects = Map.Make(Pair)

  module Types = Map.Make(Widget_type)

  module Domains = Map.Make(Domain)

  let widget x = snd @@ S.to_widget x

  let aspect_to_float (a, b) = (float_of_int a) /. (float_of_int b)

  let default_aspect = function
    | Wm.Video -> 16, 9
    | Audio -> 1, 10

  let container_position i cols rows =
    { Wm.
      x = float_of_int (i mod rows) /. float_of_int cols
    ; y = float_of_int (i / rows) /. float_of_int cols
    ; w = 1. /. float_of_int rows
    ; h = 1. /. float_of_int cols
    }

  let make_container ~cols ~rows ~video_asp ~audio_asp index (_domain, (v, a)) =
    let position = container_position index cols rows in
    let vwidth = video_asp /. (video_asp +. audio_asp) in
    let awidth = 1. -. vwidth in
    let v = Option.map (S.set_position { x = 0.; y = 0.; w = vwidth; h = 1. }) v in
    let a = Option.map (S.set_position { x = vwidth; y = 0.; w = awidth; h = 1. }) a in
    match v, a with
    | None, None ->
      "", { Wm. position; widgets = [] }
    | Some x, None | None, Some x->
      S.container_title x, { position; widgets = [S.to_widget x] }
    | Some x, Some y ->
      S.container_title x, { position; widgets = [S.to_widget x; S.to_widget y] }

  let get_primary_aspect typ (widgets : S.t list Types.t Domains.t) =
    let aspects =
      Aspects.bindings
      @@ Domains.fold (fun _ widgets acc ->
          match Types.find_opt typ widgets with
          | None -> acc
          | Some widgets ->
            List.fold_left (fun acc (x : S.t) ->
                match (widget x).aspect with
                | None -> acc
                | Some aspect ->
                  Aspects.update aspect (function
                      | None -> Some 1
                      | Some x -> Some (succ x)) acc)
              acc widgets) widgets Aspects.empty in
    match aspects with
    | [] -> default_aspect typ
    | (aspect, _) :: _ -> aspect

  let get_pairs widgets =
    Domains.map (fun widgets ->
        (* rev to take first selected widget *)
        let video = Option.map List.rev @@ Types.find_opt Video widgets in
        let audio = Option.map List.rev @@ Types.find_opt Audio widgets in
        match video, audio with
        | Some (v :: _), Some (a :: _) -> Some v, Some a
        | None, Some (a :: _) | Some [], Some (a :: _) -> None, Some a
        | Some (v :: _), None | Some (v :: _), Some [] -> Some v, None
        | _ -> None, None) widgets

  let widgets data =
    List.fold_left (fun acc x ->
        let (widget : Wm.widget) = widget x in
        Domains.update widget.domain (fun acc ->
            let widgets = match acc with None -> Types.empty | Some x -> x in
            Some (Types.update widget.type_ (fun acc ->
                let widgets = match acc with None -> [] | Some x -> x in
                Some (x :: widgets)) widgets))
          acc)
      Domains.empty data

  let layout_of_widgets ~resolution = function
    | [] -> []
    | data ->
      let widgets = widgets data in
      let asp_res = aspect_to_float resolution in
      let video_asp = aspect_to_float @@ get_primary_aspect Video widgets in
      let audio_asp = aspect_to_float @@ get_primary_aspect Audio widgets in
      let total_asp = video_asp +. audio_asp in
      let av_pairs = get_pairs widgets in
      let n = float_of_int @@ Domains.cardinal av_pairs in
      let rows = int_of_float @@ Float.round @@ sqrt n /. asp_res *. total_asp in
      let cols = int_of_float @@ Float.round @@ n /. float_of_int rows in
      List.mapi (make_container ~cols ~rows ~video_asp ~audio_asp)
      @@ Domains.bindings av_pairs
end

module Layout = Make(struct
    type t = Branches.data

    let container_title (x : t) = x.service_name

    let to_widget (x : t) = x.widget

    let set_position (p : Wm.position) (x : t) =
      let widget = { (snd @@ to_widget x) with position = Some p } in
      { x with widget = (fst x.widget, widget) }
  end)

class t ~resolution ~treeview (elt : Dom_html.element Js.t) () =
  object
    inherit Dialog.t elt ()

    val mutable resolution = resolution
    val mutable _treeview : Treeview.t = treeview

    method value : Wm.t =
      let data =
        List.filter_map (fun x ->
            match _treeview#node_value x with
            | None -> None
            | Some json ->
              try
                match Branches.data_of_yojson @@ Yojson.Safe.from_string json with
                | Error _ -> None
                | Ok x -> Some x
              with _ -> None)
        @@ _treeview#selected_leafs
      in
      { Wm.
        resolution
      ; widgets = []
      ; layout = Layout.layout_of_widgets ~resolution data
      }

    (* TODO implement *)
    method notify : event -> unit = function
      | `Streams _streams -> ()
      | `Layout layout -> resolution <- layout.resolution
  end

let make
    (structure : Structure.Annotated.t)
    (wm : Wm.Annotated.t) =
  let content = to_content structure wm in
  let actions =
    List.map Tyxml_js.Of_dom.of_button
      [ Dialog.make_action ~action:Close ~label:"Отмена" ()
      ; Dialog.make_action ~action:Accept ~label:"Применить" ()
      ] in
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
