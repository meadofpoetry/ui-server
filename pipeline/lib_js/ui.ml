open Containers
open Components
open Qoe_errors
   
(* module Plots = struct
 * 
 *   let colors = Array.init 100 (fun _ -> Random.run (Random.int 255),
 *                                         Random.run (Random.int 255),
 *                                         Random.run (Random.int 255))
 *              
 *   type plot_meta = { stream  : int
 *                    ; channel : int
 *                    ; pid     : int
 *                    ; desc    : string
 *                    }
 * 
 *   let to_plot_meta (sl : Structure.t list) =
 *     let of_stream (s : Structure.t) =
 *       let str = s.structure in
 *       let stream = Int32.to_int str.id in
 *       (\* let stream_desc = "Поток " ^ str.uri in *\)
 *       let of_channel (c : Structure.channel) =
 *         let channel_desc = c.service_name in
 *         let of_pid (p : Structure.pid) =
 *           if not p.to_be_analyzed
 *           then None
 *           else Some { stream
 *                     ; channel = c.number
 *                     ; pid = p.pid
 *                     ; desc = Printf.sprintf "%s, PID: %d" channel_desc p.pid
 *                  }
 *         in
 *         List.filter_map of_pid c.pids
 *       in
 *       List.concat @@ List.map of_channel str.channels
 *     in
 *     List.concat @@ List.map of_stream sl
 * 
 *   module M = CCMap.Make(struct
 *                  type t = int * int * int
 *                  let compare ((l1, l2, l3) : t) ((r1, r2, r3) : t) =
 *                    let c1 = compare l1 r1 in
 *                    if not (c1 = 0) then c1
 *                    else let c2 = compare l2 r2 in
 *                         if not (c2 = 0) then c2
 *                         else compare l3 r3
 *                end)
 * 
 *   let chart ~typ ~metas ~(extract : Video_data.errors -> float) ~y_max ~y_min ~e () =
 *     let open Chartjs.Line in
 *     let pairs = List.mapi (fun idx pm -> ((pm.stream, pm.channel, pm.pid), idx),
 *                                          { data = []; label = pm.desc } )
 *                   metas in
 *     let t, data = List.split pairs in
 *     let table  = M.of_list t in
 *     let config = new Config.t
 *                    ~x_axis:(Time ("my-x-axis",Bottom,Unix,Some 40000L))
 *                    ~y_axis:(Linear ("my-y-axis",Left,typ,None))
 *                    ~data
 *                    ()
 *     in
 *     let chart = new t ~config () in
 *     config#options#y_axis#ticks#set_max y_max;
 *     config#options#y_axis#ticks#set_min y_min;
 *     config#options#elements#point#set_radius 0;
 *     config#options#x_axis#ticks#set_auto_skip_padding 2;
 *     List.iteri (fun id x ->
 *         let r, g, b = colors.( id mod Array.length colors ) in
 *         x#set_background_color @@ Color.rgb r g b;
 *         x#set_border_color @@ Color.rgb r g b;
 *         x#set_line_tension 0.;
 *         x#set_cubic_interpolation_mode Monotone;
 *         x#set_fill Disabled) config#datasets;
 *     let _ = React.E.map (fun (id,data) ->
 *                 let open Video_data in
 *                 let open Option in
 *                 (M.get id table >|= fun id ->
 *                  List.get_at_idx id chart#config#datasets >|= fun ds ->
 *                  let data = { x = Int64.(Common.Time.Useconds.to_useconds data.black.timestamp / 1000L); y = extract data } in
 *                  ds#push data;
 *                  chart#update (Some { duration = Some 0
 *                                     ; is_lazy  = None
 *                                     ; easing   = None
 *                 }))
 *                 |> ignore) e in
 *     chart
 * 
 *   let chart_card ~typ ~title ~extract ~metas  ~y_max ~y_min ~e () =
 *     (\* let title = new Card.Title.t ~title () in
 *      * let prim  = new Card.Primary.t ~widgets:[title] () in *\)
 *     let media = new Card.Media.t ~widgets:[chart ~typ ~metas ~extract
 *                                                  ~y_max:(Some y_max) ~y_min:(Some y_min) ~e ()] ()
 *     in
 *     new Card.t ~widgets:[ media ] ()
 * 
 *   let create
 *         ~(init:   Structure.t list)
 *         ~(events: Structure.t list React.event)
 *         ~(data:   Video_data.t React.event) =
 *     let id  = "plot-place" in
 *     let div = Dom_html.createDiv Dom_html.document in
 *     let make (str : Structure.t list) =
 *       let open Video_data in
 *       let metas = to_plot_meta str in
 *       let e = React.E.map (fun d -> ((d.stream, d.channel,d.pid), d.errors)) data in
 *       let froz_chart = chart_card
 *                          ~title:"Заморозка" ~typ:Float
 *                          ~metas  ~extract:(fun x -> x.freeze.params.avg)
 *                          ~y_max:100. ~y_min:0. ~e () in
 *       let blac_chart = chart_card
 *                          ~title:"Черный кадр" ~typ:Float
 *                          ~metas ~extract:(fun x -> x.black.params.avg)
 *                          ~y_max:100. ~y_min:0. ~e () in
 *       let bloc_chart = chart_card
 *                          ~title:"Блочность" ~typ:Float
 *                          ~metas ~extract:(fun x -> x.blocky.params.avg)
 *                          ~y_max:100. ~y_min:0. ~e () in
 *       let brig_chart = chart_card
 *                          ~title:"Средняя яркость" ~typ:Float
 *                          ~metas ~extract:(fun x -> x.luma.params.avg)
 *                          ~y_max:250. ~y_min:0. ~e () in
 *       let diff_chart = chart_card
 *                          ~title:"Средняя разность" ~typ:Float
 *                          ~metas ~extract:(fun x -> x.diff.params.avg)
 *                          ~y_max:250. ~y_min:0. ~e () in
 *       let cells     = List.map (fun x -> let cell = new Layout_grid.Cell.t ~widgets:[x] () in
 *                                          cell#set_span 6;
 *                                          cell#set_span_phone @@ Some 12;
 *                                          cell#set_span_tablet @@ Some 12;
 *                                          cell)
 *                         [ froz_chart#widget; blac_chart#widget; bloc_chart#widget;
 *                           brig_chart#widget; diff_chart#widget ]
 *       in
 *       new Layout_grid.t ~cells ()
 *     in
 *     let _ = React.E.map (fun s ->
 *                 (try Dom.removeChild div (Dom_html.getElementById id)
 *                  with _ -> print_endline "No el");
 *                 Dom.appendChild div (make s)#root)
 *               events
 *     in
 *     Dom.appendChild div (make init)#root;
 *     div
 *     
 * end *)

module Structure = struct

  let make_pid (pid : Structure.pid) =
    let text, stext =
      match pid.content with
      | Empty   -> "Empty " ^ pid.stream_type_name, ""
      | Audio a -> "Аудио " ^ pid.stream_type_name, Printf.sprintf "Кодек: %s; Битрейт: %s;" a.codec a.bitrate
      | Video v -> "Видео " ^ pid.stream_type_name, Printf.sprintf "Кодек: %s; Разрешение: %dx%d;"
                                                                   v.codec (fst v.resolution) (snd v.resolution)
    in
    let checkbox       = new Checkbox.t ~ripple:false () in
    checkbox#set_checked pid.to_be_analyzed;
    let s, push        = React.S.create pid.to_be_analyzed in
    let pid_s          = React.S.map (fun b -> {pid with to_be_analyzed = b}) s in
    React.S.map push checkbox#s_state |> ignore;
    let item = new Tree.Item.t ~text ~secondary_text:stext ~graphic:checkbox ~value:() () in
    item, pid_s

  let make_channel (ch : Structure.channel) =
    let text, stext =
      Printf.sprintf "%s" ch.service_name,
      Printf.sprintf "Провайдер: %s"  ch.provider_name
    in
    let wl, sl = List.split @@ List.map make_pid ch.pids in
    let ch_s   = React.S.map (fun pl -> {ch with pids = pl})
                 @@ React.S.merge ~eq:Equal.physical (fun a p -> p::a) [] sl in
    let nested = new Tree.t ~items:wl () in
    let e      = new Tree.Item.t ~text ~secondary_text:stext ~nested ~value:() ()
    in e, ch_s

  let make_structure (s : Structure.t) =
    let text, stext =
      let h = Printf.sprintf "Поток: %s" (Common.Stream.Source.to_string s.source.source.info)
      in h, (Printf.sprintf "ip: %s" @@ Common.Url.to_string s.structure.uri)
    in
    let wl, cl = List.split @@ List.map make_channel s.structure.channels in
    let st_s   = React.S.map (fun chl -> {s with structure = {s.structure with channels = chl}}) @@
                   React.S.merge ~eq:Equal.physical (fun a p -> p::a) [] cl in
    let nested = new Tree.t ~items:wl () in
    let e      = new Tree.Item.t ~text ~secondary_text:stext ~nested ~value:() ()
    in e, st_s

  let make_structure_list (sl : Structure.t list) =
    match sl with
    | [] -> let ph =
              Ui_templates.Placeholder.create_with_icon
                ~text:"Потоки не обнаружены"
                ~icon:Icon.SVG.(create_simple Path.information) () in
            ph#widget, React.S.const None
    | sl -> let wl, sl = List.split @@ List.map make_structure sl in
            let sl_s   = React.S.merge ~eq:Equal.physical (fun a p -> p::a) [] sl in
            let lst    = new Tree.t ~items:wl () in
            lst#widget, React.S.map Option.return sl_s

  let make ~(init:  Structure.t list)
           ~(event: Structure.t list React.event)
           () : (Structure.t list,unit) Ui_templates.Types.settings_block =
    let div    = Dom_html.createDiv Dom_html.document |> Widget.create in
    let make (str : Structure.t list) =
      let dis, s = make_structure_list str in
      let place  = dis in
      place,s
    in
    let s_in  = React.S.hold ~eq:(Equal.list Structure.equal) init event in
    let s_div = React.S.map ~eq:(Equal.physical) (fun s -> make s) s_in in
    let s     = React.S.switch ~eq:(Equal.option @@ Equal.list Structure.equal)
                               (React.S.map ~eq:(Equal.physical) (fun n ->
                                              div#set_empty ();
                                              let tree,n_s = n in
                                              Dom.appendChild div#root tree#root;
                                              n_s) s_div)
    in
    let post = Requests.post_structure in
    div,s,post

end

module Settings = struct

  let make_setting h (s : Settings.setting) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string h;
    let peak_en_chck       = new Checkbox.t () in
    let peak_en_field      = new Form_field.t ~label:"Пиковая ошибка" ~input:peak_en_chck () in
    let peak_field         = new Textfield.t
                               ~input_id:"peak_field"
                               ~label:"Значение"
                               ~input_type:(Widget.Float ((Some (-100.)),(Some 100.))) () in
    let cont_en_chck       = new Checkbox.t () in
    let cont_en_field      = new Form_field.t ~label:"Длительная ошибка" ~input:cont_en_chck () in
    let cont_field         = new Textfield.t
                               ~input_id:"cont_field"
                               ~label:"Значение"
                               ~input_type:(Widget.Float ((Some (-100.)),(Some 100.))) () in
    let dur_field          = new Textfield.t
                               ~input_id:"dur_field"
                               ~label:"Длительность"
                               ~input_type:(Widget.Float ((Some (-100.)),(Some 100.))) () in
    peak_en_chck#set_checked s.peak_en;
    cont_en_chck#set_checked s.cont_en;
    peak_field#set_value s.peak;
    cont_field#set_value s.cont;
    dur_field#set_value s.duration;
    let peak_box = new Vbox.t ~widgets:[peak_en_field#widget; peak_field#widget;] () in
    let cont_box = new Vbox.t ~widgets:[cont_en_field#widget; cont_field#widget;] () in
    let box      = new Vbox.t ~widgets:[(Widget.create header);
                                                 peak_box#widget;
                                                 cont_box#widget;
                                                 dur_field#widget] () in
    let signal   = React.S.l5 (fun peak_en peak cont_en cont dur ->
                       Settings.{ peak_en
                                ; peak     = Option.get_or ~default:(s.peak) peak
                                ; cont_en
                                ; cont     = Option.get_or ~default:(s.cont) cont
                                ; duration = Option.get_or ~default:(s.duration) dur } )
                     peak_en_chck#s_state peak_field#s_input
                     cont_en_chck#s_state cont_field#s_input
                     dur_field#s_input in
    box, signal
    
  let make_black (b : Settings.black) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Чёрный кадр";
    let black_w, black_s   = make_setting "Доля чёрных пикселей" b.black in
    let luma_w, luma_s     = make_setting "Средняя яркость" b.luma in
    let bpixel_field       = new Textfield.t
                               ~input_id:"bpixel_field"
                               ~label:"Чёрный пиксель"
                               ~input_type:(Widget.Integer ((Some 1),(Some 256))) () in
    bpixel_field#set_value b.black_pixel;
    let box                = new Vbox.t ~widgets:[(Widget.create header);
                                                 black_w#widget;
                                                 luma_w#widget;
                                                 bpixel_field#widget] () in
    let signal             = React.S.l3 (fun black luma bpixel ->
                                 match bpixel with
                                 | None -> Settings.{ black; luma; black_pixel = b.black_pixel }
                                 | Some black_pixel -> Settings.{ black; luma; black_pixel } )
                               black_s luma_s bpixel_field#s_input in
    box, signal

  let make_freeze (f : Settings.freeze) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Заморозка видео";
    let freeze_w, freeze_s = make_setting "Доля идентичных пикселей" f.freeze in
    let diff_w, diff_s     = make_setting "Средняя разность" f.diff in
    let pixeld_field       = new Textfield.t
                               ~input_id:"pixeld_field"
                               ~label:"Идентичный пиксель"
                               ~input_type:(Widget.Integer ((Some 1),(Some 256))) () in
    pixeld_field#set_value f.pixel_diff;
    let box                = new Vbox.t ~widgets:[(Widget.create header);
                                                 freeze_w#widget;
                                                 diff_w#widget;
                                                 pixeld_field#widget] () in
    let signal             = React.S.l3 (fun freeze diff pixeld ->
                                 match pixeld with
                                 | None -> Settings.{ freeze; diff; pixel_diff = f.pixel_diff }
                                 | Some pixel_diff -> Settings.{ freeze; diff; pixel_diff } )
                               freeze_s diff_s pixeld_field#s_input in
    box, signal

  let make_blocky (b : Settings.blocky) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Блочность";
    let blocky_w, blocky_s = make_setting "Блочность" b.blocky in
    let mark_chck          = new Checkbox.t () in
    let mark_field         = new Form_field.t ~label:"Визуализировать блоки" ~input:mark_chck () in
    mark_chck#set_checked b.mark_blocks;
    let box                = new Vbox.t ~widgets:[(Widget.create header);
                                                 blocky_w#widget;
                                                 mark_field#widget] () in
    let signal             = React.S.l2 (fun blocky markb -> Settings.{ blocky; mark_blocks = markb })
                               blocky_s mark_chck#s_state in
    box, signal
    
  let make_video (v : Settings.video) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Видео";
    let loss_field         = new Textfield.t
                               ~input_id:"loss_field"
                               ~label:"Пропадание видео"
                               ~input_type:(Widget.Float ((Some 0.),(Some 1.))) () in
    loss_field#set_value v.loss;
    let black_w, black_s   = make_black v.black in
    let freeze_w, freeze_s = make_freeze v.freeze in
    let blocky_w, blocky_s = make_blocky v.blocky in
    let box                = new Vbox.t ~widgets:[(Widget.create header);
                                                 loss_field#widget;
                                                 black_w#widget;
                                                 freeze_w#widget;
                                                 blocky_w#widget] () in
    let signal             = React.S.l4 (fun loss black freeze blocky ->
                                 match loss with
                                 | None -> Settings.{ loss = v.loss; black; freeze; blocky }
                                 | Some loss -> Settings.{ loss; black; freeze; blocky } )
                               loss_field#s_input black_s freeze_s blocky_s in
    box, signal

  let make_silence (s : Settings.silence) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Тишина";
    let sil_w, sil_s       = make_setting "Громкость" s.silence in
    let box                = new Vbox.t ~widgets:[(Widget.create header); sil_w#widget] () in
    let signal             = React.S.map (fun silence -> Settings.{ silence } ) sil_s in
    box, signal

  let make_loudness (s : Settings.loudness) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Перегрузка звука";
    let sil_w, sil_s       = make_setting "Громкость" s.loudness in
    let box                = new Vbox.t ~widgets:[(Widget.create header); sil_w#widget] () in
    let signal             = React.S.map (fun loudness -> Settings.{ loudness } ) sil_s in
    box, signal

  let make_adv (s : Settings.adv) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Рекламные вставки";
    let diff               = new Textfield.t
                               ~input_id:"diff"
                               ~label:"diff"
                               ~input_type:(Widget.Float ((Some 0.),(Some 100.))) () in
    let buf                = new Textfield.t
                               ~input_id:"buf"
                               ~label:"buf"
                               ~input_type:(Widget.Integer ((Some 0),(Some 100))) () in
    diff#set_value s.adv_diff;
    buf#set_value s.adv_buf;
    let box                = new Vbox.t ~widgets:[(Widget.create header); diff#widget; buf#widget] () in
    let signal             = React.S.l2 (fun diff buf ->
                                 Settings.{ adv_diff = Option.get_or ~default:(s.adv_diff) diff
                                          ; adv_buf  = Option.get_or ~default:(s.adv_buf) buf })
                               diff#s_input buf#s_input in
    box, signal

  let make_audio (a : Settings.audio) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Аудио";
    let loss_field         = new Textfield.t
                               ~input_id:"loss_field"
                               ~label:"Пропадание аудио"
                               ~input_type:(Widget.Float ((Some 0.),(Some 1.))) () in
    loss_field#set_value a.loss;
    let silence_w, sil_s   = make_silence  a.silence in
    let loudness_w, loud_s = make_loudness a.loudness in
    let adv_w, adv_s       = make_adv      a.adv in
    let box                = new Vbox.t ~widgets:[(Widget.create header);
                                                 loss_field#widget;
                                                 silence_w#widget;
                                                 loudness_w#widget;
                                                 (* adv_w#widget *)] () in
    let signal             = React.S.l4 (fun loss silence loudness adv ->
                                 match loss with
                                 | None -> Settings.{ loss = a.loss; silence; loudness; adv }
                                 | Some loss -> Settings.{ loss; silence; loudness; adv } )
                               loss_field#s_input sil_s loud_s adv_s in
    box, signal

  let make_layout (s : Settings.t) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Настройки";
    let v, v_s = make_video s.video in
    let a, a_s = make_audio s.audio in
    let s      = React.S.l2 (fun v a -> Some Settings.{ video = v; audio = a; }) v_s a_s in
    let box    = new Vbox.t ~widgets:[(Widget.create header); v#widget; a#widget] () in
    box, s

  let make ~(init:  Settings.t)
           ~(event: Settings.t React.event)
           () : (Settings.t,unit) Ui_templates.Types.settings_block =
    let div    = Dom_html.createDiv Dom_html.document |> Widget.create in
    let make (set : Settings.t) =
      let dis, s = make_layout set in
      let place  = dis in
      place,s
    in
    let s_in  = React.S.hold ~eq:Settings.equal init event in
    let s_div = React.S.map ~eq:(Equal.physical) (fun s -> make s) s_in in
    let s     = React.S.switch ~eq:(Equal.option Settings.equal)
                               (React.S.map ~eq:(Equal.physical) (fun n ->
                                              div#set_empty ();
                                              let w,n_s = n in
                                              Dom.appendChild div#root w#root;
                                              n_s) s_div)
    in
    let post = Requests.post_settings in
    div,s,post
end
