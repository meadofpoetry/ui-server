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
  let widgets = List.filter_map (fun (name, (widget : Wm.widget)) ->
      match (widget.domain : Wm.domain) with
      | (Chan {stream; channel} : Wm.domain) ->
        Some ((name, widget), ({ stream; channel } : channel))
      | (Nihil : Wm.domain) -> None) wm.widgets in
  Branches.make_streams widgets streams

module Layout = struct

  let equal_aspect (ax, ay) (bx, by) = ax = bx && ay = by

  let fmod v1 v2 =
    if v2 = 0.0 then 0.0
    else
      let val1 = floor (v1 /. v2) in
      v1 -. val1 *. v2

  module Aspects = Map.Make(struct
      type t = int * int
      let compare = compare
    end)

  let get_all_aspects (widgets : Wm.widget list) =
    List.sort (fun (_, a) (_, b) -> compare a b)
    @@ Aspects.bindings
    @@ List.fold_left (fun acc (x : Wm.widget) ->
        match x.aspect with
        | None -> acc
        | Some aspect ->
          Aspects.update aspect (function
              | None -> Some 1
              | Some x -> Some (succ x)) acc)
      Aspects.empty widgets

  let get_main_aspect ~default widgets =
    match get_all_aspects widgets with
    | [] -> default
    | (asp, _) :: _ -> asp

  let compare_by_domain (a : Wm.widget as 'a) (b : 'a) =
    match a.domain, b.domain with
    | Wm.Nihil, Nihil -> 0
    | Nihil, Chan _ -> -1
    | Chan _, Nihil -> 1
    | Chan a, Chan b ->
      match Stream.ID.compare a.stream b.stream with
      | 0 -> compare a.channel b.channel
      | x -> x

  let get_widgets_uniq_chnl_strm (widgets : Wm.widget list) =
    List.sort_uniq compare_by_domain widgets

  let get_av_widget_pairs
      (video_widgets : Wm.widget list)
      (audio_widgets : Wm.widget list) =
    let rec aux acc = function
      | [] -> acc
      | hd :: tl ->
        let equal a b = compare_by_domain a b = 0 in
        let acc = match List.find_opt (equal hd) audio_widgets with
          | None -> acc
          | Some a -> (Some hd, Some a) :: acc
        in
        aux acc tl
    in
    aux [] video_widgets

  let get_non_paired_widgets
      (widgets : Wm.widget list)
      (selector : Wm.widget_type)
      (av_pairs : (Wm.widget option * Wm.widget option) list) =
    let rec aux acc
        (av_pairs: (Wm.widget option * Wm.widget option) list) = function
      | [] -> acc
      | hd :: tl ->
        let res = List.find_opt
            (fun (v, a) -> match selector with
               | Video -> (match v with
                   | None -> false
                   | Some x -> compare_by_domain x hd = 0)
               | Audio -> (match a with
                   | None -> false
                   | Some y -> compare_by_domain y hd = 0))
            av_pairs in
        let acc = match res with
          | Some _va -> acc
          | None ->
            match selector with
            | Video -> (Some hd, None) :: acc
            | Audio -> (None, Some hd) :: acc
        in
        aux acc av_pairs tl
    in
    aux [] av_pairs widgets

  let generate_containers
      ~(rows : int)
      ~(cols : int)
      ~(video_asp : float)
      ~(audio_asp : float)
      (av_pairs : (Wm.widget option * Wm.widget option) list) =
    let rows = float_of_int rows in
    let cols = float_of_int cols in
    let video_asp = 1.0 /. video_asp in
    let audio_asp = 1.0 /. audio_asp in
    List.mapi (fun index (v, a) ->
        let (container_pos : Wm.position) =
          { x = (fmod (float_of_int index) rows) /. cols
          ; y = floor (float_of_int index /. rows) /. cols
          ; w = 1.0 /. rows
          ; h = 1.0 /. cols
          } in
        let wv = match v with
          | None -> None
          | Some (w : Wm.widget) ->
            let position =
              Some { Wm.
                     x = 0.0
                   ; y = 0.0
                   ; w = video_asp /. (video_asp +. audio_asp)
                   ; h = 1.0 }
            in
            Some { w with position }
        in
        let wa = match a with
          | None -> None
          | Some (w : Wm.widget) ->
            let position =
              Some { Wm.
                     x = video_asp /. (video_asp +. audio_asp)
                   ; y = 0.0
                   ; w = 1.0 -. video_asp /. (video_asp +. audio_asp)
                   ; h = 1.0
                   }
            in
            Some { w with position }
        in
        match wv, wa with
        | None, None ->
          "", { Wm. position = container_pos; widgets = []}
        | Some x, None ->
          x.description,
          { position = container_pos; widgets = [x.description, x] }
        | None, Some y ->
          y.description,
          { position = container_pos; widgets = [y.description, y] }
        | Some x, Some y ->
          x.description,
          { position = container_pos
          ; widgets = [x.description, x; y.description, y]
          }) av_pairs

  let aspect_to_float (x, y) =
    float_of_int x /. float_of_int y

  let split_widgets data =
    List.fold_left (fun (video, audio) v ->
        let (widget : Wm.widget) = snd v(* .widget *) in
        match widget.domain with
        | Wm.Nihil -> video, audio
        | Chan _ ->
          match widget.type_ with
          | Video -> widget :: video, audio
          | Audio -> video, widget :: audio) ([], []) data

  let layout_of_widgets ~resolution data =
    let video_widgets, audio_widgets = split_widgets data in
    let video_main_aspect =
      get_main_aspect
        ~default:(16, 9)
        video_widgets in
    let audio_main_aspect =
      aspect_to_float
      @@ get_main_aspect
        ~default:(1, 10)
        audio_widgets in
    let main_aspect = 1.0 /. ( 1.0 /. aspect_to_float video_main_aspect +. 1.0 /. audio_main_aspect) in
    let video_unique = get_widgets_uniq_chnl_strm video_widgets in
    let audio_unique = get_widgets_uniq_chnl_strm audio_widgets in
    let av_pairs = get_av_widget_pairs video_unique audio_unique in
    let all_pairs =
      av_pairs
      @ get_non_paired_widgets video_unique Video av_pairs
      @ get_non_paired_widgets audio_unique Audio av_pairs in
    let n = List.length all_pairs in
    let asp_res = aspect_to_float resolution in
    let rows =
      int_of_float
      @@ Float.round
      @@ sqrt (float_of_int n) *. asp_res /. main_aspect in
    let cols =
      int_of_float
      @@ Float.round
      @@ float_of_int n /. float_of_int (if rows > 0 then rows else 1) in
    print_endline @@ Printf.sprintf "v=%d, vasp=%dx%d, a=%d, aasp=%g, all=%d, rows=%d, cols=%d"
      (List.length video_unique)
      (fst video_main_aspect)
      (snd video_main_aspect)
      (List.length audio_unique)
      audio_main_aspect
      (List.length all_pairs)
      rows cols;
    if rows <= 0 || cols <= 0 || n <= 0
    then []
    else generate_containers
        ~rows
        ~cols
        ~video_asp:(aspect_to_float video_main_aspect)
        ~audio_asp:audio_main_aspect
        all_pairs

end

class t ~resolution ~treeview (elt : Dom_html.element Js.t) () =
  object
    inherit Dialog.t elt ()

    val mutable resolution = resolution
    val mutable _treeview : Treeview.t = treeview

    method value =
      let _data =
        List.filter_map (fun x ->
            match _treeview#node_value x with
            | None -> None
            | Some json ->
              try
                match Branches.data_of_yojson @@ Yojson.Safe.from_string json with
                | Error _ -> None
                | Ok x -> Some x
              with _ -> None)
        @@ _treeview#selected_leafs in
      Layout.layout_of_widgets ~resolution [] (* data *)

    (* TODO implement *)
    method notify : event -> unit = function
      | `Streams _streams -> ()
      | `Layout layout -> resolution <- layout.resolution
  end

let make
    (streams : Structure.Annotated.t)
    (wm : Wm.Annotated.t) =
  let content = to_content streams wm in
  let accept = Dialog.make_action ~action:Accept ~label:"Применить" () in
  let actions =
    List.map Tyxml_js.Of_dom.of_button
      [ Dialog.make_action ~action:Close ~label:"Отмена" ()
      ; accept ] in
  let _l = Js_of_ocaml_lwt.Lwt_js_events.clicks accept (fun _ _ ->
      print_endline @@ string_of_int @@ List.length wm.widgets;
      let layout = Layout.layout_of_widgets ~resolution:wm.resolution wm.widgets in
      let wm =
        { Wm.
          widgets = []
        ; layout
        ; resolution = wm.resolution
        } in
      print_endline
      @@ Yojson.Safe.to_string
      @@ Wm.to_yojson wm;
      Lwt.return_unit) in
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
