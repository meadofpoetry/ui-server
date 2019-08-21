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
      layout_of_widgets ~resolution data

    (* TODO implement *)
    method notify : event -> unit = function
      | `Streams _streams -> ()
      | `Layout layout -> resolution <- layout.resolution
  end

(* module LayoutOfWidget = struct
 * 
 *   let equal_aspect (ax, ay) (bx, by) = ax = bx && ay = by
 * 
 *   let fmod v1 v2 =
 *     if v2 = 0.0 then 0.0
 *     else
 *       let val1 = floor (v1 /. v2) in
 *       v1 -. val1 *. v2
 * 
 *   module Aspects = Map.Make(struct
 *       type t = int * int
 *       let compare = compare
 *     end)
 * 
 *   let get_all_aspects (widgets : Wm.widget list) =
 *     List.sort (fun (_, a) (_, b) -> compare a b)
 *     @@ Aspects.bindings
 *     @@ List.fold_left (fun acc (x : Wm.widget) ->
 *         match x.aspect with
 *         | None -> acc
 *         | Some aspect ->
 *           Aspects.update aspect (function
 *               | None -> Some 1
 *               | Some x -> Some (succ x)) acc)
 *       Aspects.empty widgets
 * 
 *   (\* Возвращает первый элемент из списка aspects_weighted_sorted, с проверкой на то что есть
 *      хотябы 1 элемент в списке. Иначе возвращает аспект по-умолчанию *\)
 *   let get_main_aspect (aspects_weighted_sorted : ((int * int) * int) list)
 *       (default_aspect : (int * int)) =
 *     match aspects_weighted_sorted with
 *     | [] -> default_aspect
 *     | (asp, _) :: _ ->
 *       if equal_aspect asp (0, 0)
 *       then default_aspect
 *       else asp
 * 
 *   let compare_widgets_by_strm_chnl (a : Wm.widget) (b : Wm.widget) =
 *     let ((a_stream: Stream.ID.t option), a_channel) =
 *       match (a.domain : Wm.domain) with
 *       | Nihil -> (None, (-1))
 *       | Chan x -> (Some x.stream, x.channel)
 *     in
 *     let (b_stream : Stream.ID.t option), b_channel = match b.domain with
 *       | Wm.Nihil -> None, (-1)
 *       | Chan x -> Some x.stream, x.channel
 *     in
 *     let stream_compared =
 *       match a_stream, b_stream with
 *       | None, None -> 0
 *       | Some _, None -> 1
 *       | None, Some _ -> 1
 *       | Some x, Some y -> compare x y
 *     in
 *     let channel_compared =
 *       if a_channel = b_channel then 0
 *       else if a_channel > b_channel then 1
 *       else -1
 *     in
 *     if stream_compared = 0 && channel_compared = 0 then 0
 *     else if stream_compared > 0 && channel_compared > 0 then 1
 *     else if stream_compared < 0 && channel_compared > 0 then 1
 *     else -1
 * 
 *   let get_widgets_uniq_chnl_strm (widgets : Wm.widget list) =
 *     List.sort_uniq compare_widgets_by_strm_chnl widgets
 * 
 *   (\* Формирует пары из видео и аудио виджетов с одинаковым каналом и потоком*\)
 *   let get_av_widget_pairs
 *       (video_widgets_uniq_chnl_strm : Wm.widget list)
 *       (audio_widgets_uniq_chnl_strm : Wm.widget list) =
 *     let rec aux acc audio_widgets_uniq_chnl_strm = function
 *       | [] -> acc
 *       | hd :: tl ->
 *         let res = List.find_opt (fun w ->
 *             compare_widgets_by_strm_chnl hd w = 0)
 *             audio_widgets_uniq_chnl_strm in
 *         let acc = match res with
 *           | None -> acc
 *           | Some a -> (Some hd, Some a) :: acc
 *         in
 *         aux acc audio_widgets_uniq_chnl_strm tl
 *     in
 *     aux [] audio_widgets_uniq_chnl_strm video_widgets_uniq_chnl_strm
 * 
 *   (\* Ищет видео/аудио виджеты без аудио/видео пары*\)
 *   let get_non_paired_av_widgets
 *       (widgets_with_uniq_chnl_strm : Wm.widget list)
 *       (selector : Wm.widget_type)
 *       (av_pairs : (Wm.widget option * Wm.widget option) list) =
 *     let rec aux acc
 *         (av_pairs: (Wm.widget option * Wm.widget option) list) = function
 *       | [] -> acc
 *       | hd :: tl ->
 *         let res = List.find_opt
 *             (fun (v, a) -> match selector with
 *                | Video -> (match v with
 *                    | None -> false
 *                    | Some x -> compare_widgets_by_strm_chnl x hd = 0)
 *                | Audio -> (match a with
 *                    | None -> false
 *                    | Some y -> compare_widgets_by_strm_chnl y hd = 0))
 *             av_pairs in
 *         let acc = match res with
 *           | Some _va -> acc
 *           | None ->
 *             match selector with
 *             | Video -> (Some hd, None) :: acc
 *             | Audio -> (None, Some hd) :: acc
 *         in
 *         aux acc av_pairs tl
 *     in
 *     aux [] av_pairs widgets_with_uniq_chnl_strm
 * 
 *   (\* Итоговое создание контейнеров *\)
 *   let generate_containers
 *       ~(nx : float)
 *       ~(ny : float)
 *       ~(video_asp : float)
 *       ~(audio_asp : float)
 *       (av_pairs : (Wm.widget option * Wm.widget option) list) =
 *     let rec aux
 *         (acc : (string * Wm.container) list)
 *         (x : float) (\* initial 0.0 *\)
 *         (y : float) (\* initial 0.0 *\)
 *         (index : float) (\* initial 0.0 *\)
 *         (video_asp_inv : float)
 *         (audio_asp_inv : float) = function
 *       | [] -> acc
 *       | (v, a) :: tl ->
 *         let (container_pos : Wm.position) =
 *           { x = (fmod index nx) /. nx
 *           ; y = floor (index /. nx) /. ny
 *           ; w = 1.0 /. nx
 *           ; h = 1.0 /. ny
 *           } in
 *         let wv = match v with
 *           | None -> None
 *           | Some (w : Wm.widget) ->
 *             let position =
 *               Some { Wm.
 *                      x = 0.0
 *                    ; y = 0.0
 *                    ; w = video_asp_inv /. (video_asp_inv +. audio_asp_inv)
 *                    ; h = 1.0 }
 *             in
 *             Some { w with position }
 *         in
 *         let wa = match a with
 *           | None -> None
 *           | Some (w : Wm.widget) ->
 *             let position =
 *               Some { Wm.
 *                      x = video_asp_inv /. (video_asp_inv +. audio_asp_inv)
 *                    ; y = 0.0
 *                    ; w = 1.0 -. video_asp_inv /. (video_asp_inv +. audio_asp_inv)
 *                    ; h = 1.0
 *                    }
 *             in
 *             Some { w with position }
 *         in
 *         let container = match wv, wa with
 *           | None, None ->
 *             "", { Wm. position = container_pos; widgets = []}
 *           | Some x, None ->
 *             x.description,
 *             { position = container_pos; widgets = [x.description, x] }
 *           | None, Some y ->
 *             y.description,
 *             { position = container_pos; widgets = [y.description, y] }
 *           | Some x, Some y ->
 *             x.description,
 *             { position = container_pos
 *             ; widgets = [x.description, x; y.description, y]
 *             }
 *         in
 *         aux (container :: acc) x y (index +. 1.0)
 *           video_asp_inv
 *           audio_asp_inv
 *           tl
 *     in
 *     if nx > 0.0 && ny > 0.0
 *     then aux [] 0.0 0.0 0.0 (1.0 /. video_asp) (1.0 /. audio_asp) av_pairs
 *     else []
 * 
 *   let default_video_aspect = 16, 9
 * 
 *   let default_audio_aspect = 1, 10
 * 
 *   let split_widgets data =
 *     List.fold_left (fun (video, audio) (v : Branches.data) ->
 *         let (widget : Wm.widget) = snd v.widget in
 *         match widget.domain with
 *         | Wm.Nihil -> video, audio
 *         | Chan _ ->
 *           match widget.type_ with
 *           | Video -> widget :: video, audio
 *           | Audio -> video, widget :: audio) ([], []) data
 * 
 *   let layout_of_widgets ~resolution (data : Branches.data list) =
 *     let video_widgets, audio_widgets = split_widgets data in
 *     (\* Считаем количество одинаковых аспектов и
 *        сортируем аспекты по их количеству, первым большее количество. *\)
 *     let video_uniq_aspects_weight_sorted =
 *       get_all_aspects video_widgets in
 *     (\* Получаем наиболее часто используемый аспект для видео *\)
 *     let video_main_aspect = get_main_aspect
 *         video_uniq_aspects_weight_sorted default_video_aspect in
 *     (\* Считаем количество одинаковых аспектов и
 *        сортируем аспекты по их количеству, первым большее количество. *\)
 *     let audio_uniq_aspects_weight_sorted =
 *       get_all_aspects audio_widgets in
 *     (\* Получаем наиболее часто используемый аспект для аудио *\)
 *     let audio_main_aspect = get_main_aspect
 *         audio_uniq_aspects_weight_sorted default_audio_aspect in
 *     (\* Вычисляем главный аспект, как сумму длин по ширине для 1 видео и 1 аудио виджета,
 *        деленые на высоту и результат инвертируем (для того чтобы можно было использовать
 *        функцию из position:get_float_aspect (хотя она объявлена в LayoutOfWidget) - она такая же)*\)
 *     let main_aspect = 1.0 /. ( 1.0 /. video_main_aspect +. 1.0 /. audio_main_aspect) in
 *     (\* Оставляем виджеты у которых уникальный канал и поток *\)
 *     let video_widgets_with_uniq_chnl_strm = get_widgets_uniq_chnl_strm video_widgets in
 *     let audio_widgets_with_uniq_chnl_strm = get_widgets_uniq_chnl_strm audio_widgets in
 *     (\* Находим пары видео и аудио виджетов *\)
 *     let av_pairs = get_av_widget_pairs video_widgets_with_uniq_chnl_strm audio_widgets_with_uniq_chnl_strm in
 *     (\* Находим видео без аудио пары *\)
 *     let v_non_paired = get_non_paired_av_widgets video_widgets_with_uniq_chnl_strm Video av_pairs in
 *     (\* Находим аудио без видео пары *\)
 *     let a_non_paired = get_non_paired_av_widgets audio_widgets_with_uniq_chnl_strm Audio av_pairs in
 *     (\* Объединяем все виджеты с парами и без *\)
 *     let common_av_pairs = av_pairs @ v_non_paired @ a_non_paired in
 *     let n = List.length common_av_pairs in
 *     (\* Аспект 'экрана' *\)
 *     let asp_res = float_of_int (fst resolution) /. float_of_int (snd resolution) in
 *     (\* Высчитыаем количество колонок и рядов в зависимости от аспекта экрана и основного аспекта *\)
 *     let nx = int_of_float (Float.round (sqrt (float_of_int n) *. asp_res /. main_aspect)) in
 *     let ny = int_of_float (Float.round (float_of_int n /. (float_of_int (if nx > 0 then nx else 1)))) in
 *     if nx <= 0 || ny <= 0 || n <= 0
 *     then []
 *     else generate_containers
 *         ~nx:(float_of_int nx)
 *         ~ny:(float_of_int ny)
 *         ~video_asp:video_main_aspect
 *         ~audio_asp:audio_main_aspect
 *         common_av_pairs
 * 
 * end *)

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
