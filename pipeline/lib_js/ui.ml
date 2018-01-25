open Components

module Plots = struct

  let colors = Array.init 100 (fun _ -> Random.int 255, Random.int 255, Random.int 255)
  
  type plot_meta = { stream  : int
                   ; channel : int
                   ; pid     : int
                   ; desc    : string
                   }

  let to_plot_meta (sl : Structure.t list) =
    let of_stream (s : Structure.t) =
      let str = s.structure in
      let stream = Int32.to_int str.id in
      let stream_desc = "Поток " ^ str.uri in
      let of_channel (c : Structure.channel) =
        let channel = c.number in
        let channel_desc = Printf.sprintf "%d %s (%s)"
                             channel
                             c.service_name
                             c.provider_name in
        let of_pid (p : Structure.pid) =
          if not p.to_be_analyzed
          then None
          else Some { stream
                    ; channel
                    ; pid = p.pid
                    ; desc = Printf.sprintf "%s %s, pid: %d" stream_desc channel_desc p.pid
                 }
        in
        CCList.filter_map of_pid c.pids
      in
      CCList.concat @@ CCList.map of_channel str.channels
    in
    CCList.concat @@ CCList.map of_stream sl

  module M = CCMap.Make(struct
                 type t = int * int * int
                 let compare (l : t) (r : t) = compare l r
               end)
    
  let chart ~typ ~metas ~(extract : Video_data.params -> float) ~y_max ~y_min ~e () =
    let open Chartjs.Line in
    let filter_data ds =
      let sz, sum  = List.fold_left (fun (sz, sum) d -> succ sz, sum +. d.y) (0, 0.) ds in
      let szf      = float_of_int sz in
      let mean     = sum /. szf in
     (* let dev      = List.fold_left (fun acc d -> acc +. abs_float (d.y -. mean)) 0. ds in
      let mean_dev = dev /. szf in*)
      let mean_v   = CCList.get_at_idx_exn (sz / 2) ds in
      [ { mean_v with y = mean } ](* :: (List.filter (fun d -> abs_float (d.y -. mean) > mean_dev *. 4.) ds)*)
    in
    let pairs = List.mapi (fun idx pm -> ((pm.stream, pm.channel, pm.pid), idx),
                                         { data = []; label = pm.desc } )
                  metas in
    let t, data = List.split pairs in
    let table  = M.of_list t in
    let config = new Config.t
                   ~x_axis:(Time ("my-x-axis",Bottom,Unix,Some 40000L))
                   ~y_axis:(Linear ("my-y-axis",Left,typ,None))
                   ~data
                   ()
    in
    let chart = new t ~config () in
    config#options#y_axis#ticks#set_max y_max;
    config#options#y_axis#ticks#set_min y_min;
    config#options#x_axis#ticks#set_auto_skip_padding 2;
    List.iteri (fun id x ->
        let r, g, b = colors.( id mod Array.length colors ) in
        x#set_background_color @@ Color.rgb r g b;
        x#set_cubic_interpolation_mode Monotone;
        x#set_fill Disabled) config#datasets;
    let _ = React.E.map (fun (id,data) ->
                let open Video_data in
                let open CCOpt in
                (M.get id table >|= fun id ->
                 CCList.get_at_idx id chart#config#datasets >|= fun ds ->
                 let data = List.map (fun (p : params) -> { x = Int64.(div p.time 1000L); y = extract p } ) data in
                 let data = filter_data data in
                 ds#append data;
                 chart#update (Some { duration = Some 0
                                    ; is_lazy  = None
                                    ; easing   = None
                }))
                |> ignore) e in
    chart

  let chart_card ~typ ~title ~extract ~metas  ~y_max ~y_min ~e () =
    let title = new Card.Title.t ~title () in
    let prim  = new Card.Primary.t ~widgets:[title] () in
    let media = new Card.Media.t ~widgets:[chart ~typ ~metas ~extract  ~y_max ~y_min ~e ()] () in
    new Card.t ~sections:[ `Primary prim; `Media media ] ()

  let create
        ~(init:   Structure.t list)
        ~(events: Structure.t list React.event)
        ~(data:   Video_data.t React.event) =
    let id  = "plot-place" in
    let div = Dom_html.createDiv Dom_html.document in
    let make (str : Structure.t list) =
      let open Video_data in
      let metas = to_plot_meta str in
      let e = React.E.map (fun d -> ((d.stream, d.channel,d.pid), d.parameters)) data in
      let froz_chart = chart_card
                         ~title:"Заморозка" ~typ:Float
                         ~metas  ~extract:(fun x -> x.frozen_pix)
                          ~y_max:100. ~y_min:0. ~e () in
      let blac_chart = chart_card
                         ~title:"Черный кадр" ~typ:Float
                         ~metas ~extract:(fun x -> x.black_pix)
                          ~y_max:100. ~y_min:0. ~e () in
      let bloc_chart = chart_card
                         ~title:"Блочность" ~typ:Float
                         ~metas ~extract:(fun x -> x.blocks)
                          ~y_max:100. ~y_min:0. ~e () in
      let brig_chart = chart_card
                         ~title:"Средняя яркость" ~typ:Float
                         ~metas ~extract:(fun x -> x.avg_bright)
                          ~y_max:250. ~y_min:0. ~e () in
      let diff_chart = chart_card
                         ~title:"Средняя разность" ~typ:Float
                         ~metas ~extract:(fun x -> x.avg_diff)
                          ~y_max:250. ~y_min:0. ~e () in
      let cells     = CCList.map (fun x -> let cell = new Layout_grid.Cell.t ~widgets:[x] () in
                                           cell#set_span 6;
                                           cell#set_span_phone 12;
                                           cell#set_span_tablet 12;
                                           cell)
                        [ froz_chart#widget; blac_chart#widget; bloc_chart#widget;
                          brig_chart#widget; diff_chart#widget ]
      in
      new Layout_grid.t ~cells ()
    in
    let _ = React.E.map (fun s ->
                (try Dom.removeChild div (Dom_html.getElementById id)
                 with _ -> print_endline "No el");
                Dom.appendChild div (make s)#root)
              events
    in
    Dom.appendChild div (make init)#root;
    div
    
end

module Structure = struct

  let make_pid (pid : Structure.pid) =
    let text, stext =
      match pid.content with
      | Empty   -> "Empty " ^ pid.stream_type_name, ""
      | Audio a -> "Audio " ^ pid.stream_type_name, Printf.sprintf "Codec: %s; Bitrate: %s;" a.codec a.bitrate
      | Video v -> "Video " ^ pid.stream_type_name, Printf.sprintf "Codec: %s; Resolution: %dx%d;"
                                                                   v.codec (fst v.resolution) (snd v.resolution)
    in
    let checkbox       = new Checkbox.t ~ripple:false () in
    checkbox#set_checked pid.to_be_analyzed;
    let s, push        = React.S.create pid.to_be_analyzed in
    let pid_s          = React.S.map (fun b -> {pid with to_be_analyzed = b}) s in
    React.S.map push checkbox#s_state |> ignore;
    new Tree.Item.t ~text ~secondary_text:stext ~start_detail:checkbox (), pid_s

  let make_channel (ch : Structure.channel) =
    let text, stext =
      Printf.sprintf "Channel %d" ch.number,
      Printf.sprintf "Serv: %s; Provider: %s" ch.service_name ch.provider_name
    in
    let wl, sl = CCList.split @@ CCList.map make_pid ch.pids in
    let ch_s   = React.S.map (fun pl -> {ch with pids = pl}) @@ React.S.merge ~eq:(==) (fun a p -> p::a) [] sl in
    let nested = new Tree.t ~items:wl () in
    let e = new Tree.Item.t ~text ~secondary_text:stext ~nested ()
    in e, ch_s

  let make_structure (s : Structure.t) =
    let text, stext =
      let h = match s.source with
        | Unknown    -> Printf.sprintf "Unknown source"
        | Stream src -> Printf.sprintf "Source: %s" (CCOpt.get_or ~default:"stream" src.description)
      in h, (Printf.sprintf "ip: %s" s.structure.uri)
    in
    let wl, cl = CCList.split @@ CCList.map make_channel s.structure.channels in
    let st_s   = React.S.map (fun chl -> {s with structure = {s.structure with channels = chl}}) @@
                   React.S.merge ~eq:(==) (fun a p -> p::a) [] cl in
    let nested = new Tree.t ~items:wl () in
    let e      = new Tree.Item.t ~text ~secondary_text:stext ~nested ()
    in e, st_s

  let make_structure_list (sl : Structure.t list) =
    let wl, sl = CCList.split @@ CCList.map make_structure sl in
    let sl_s   = React.S.merge ~eq:(==) (fun a p -> p::a) [] sl in
    let lst    = new Tree.t ~items:wl () in
    lst#set_dense true;
    lst#style##.maxWidth := Js.string "400px";
    lst, sl_s

  let create
        ~(init:   Structure.t list)
        ~(events: Structure.t list React.event)
        ~(post:   Structure.t list -> unit) =
    let id  = "structure-place" in
    let div = Dom_html.createDiv Dom_html.document in
    let make (str : Structure.t list) =
      let dis, s = make_structure_list str in
      let but    = new Components.Button.t ~label:"send" () in
      let place  = new Components.Card.t
                     ~sections:[`Primary (new Card.Primary.t ~widgets:[dis] ());
                                `Actions (new Card.Actions.t ~widgets:[but] ())]
                     () in
      place#set_id id;
      but#button_element##.onclick := Dom.handler (fun _ -> post @@ React.S.value s; Js._false);
      place
    in
    let _ = React.E.map (fun s ->
                (try Dom.removeChild div (Dom_html.getElementById id)
                 with _ -> print_endline "No el");
                Dom.appendChild div (make s)#root)
              events
    in
    Dom.appendChild div (make init)#root;
    div

end

module Wm = struct

  let make_layout (wm: Wm.t) =
    Layout.initialize wm

  let create
        ~(init:   Wm.t)
        ~(events: Wm.t React.event)
        ~(post:   Wm.t -> unit) =
    let open Layout in
    let id   = "wm-widget" in
    let div  = Dom_html.createDiv Dom_html.document in
    let cell = new Layout_grid.Cell.t ~widgets:[Widget.create div] () in
    cell#set_span 12;
    let grid = new Layout_grid.t ~cells:[cell] () in
    let make (wm : Wm.t) =
      let grid,layout,f_add,f_rm = make_layout wm in
      let add     = new Button.t ~label:"Добавить"  () in
      let rm      = new Button.t ~label:"Удалить"   () in
      let apply   = new Button.t ~label:"Применить" () in
      let btn_box = new Box.t ~vertical:false ~widgets:[add; rm; apply] () in
      let box     = new Box.t ~gap:20 ~widgets:[btn_box#widget;grid#widget] () in
      let _       = f_add add#e_click in
      let _       = f_rm  rm#e_click  in
      let _       = React.E.map (fun _ -> post { wm with layout = React.S.value layout }) apply#e_click in
      box#set_id id;
      box
    in
    let _ = React.E.map (fun s ->
                (try Dom.removeChild div (Dom_html.getElementById id)
                 with _ -> print_endline "No el");
                Dom.appendChild div (make s)#root) events
    in
    Dom.appendChild div (make init)#root;
    grid#root

end

module Settings = struct

  let make_setting h (s : Settings.setting) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string h;
    let peak_en_chck       = new Checkbox.t () in
    let peak_en_field      = new Form_field.t ~label:"Peak enable" ~input:peak_en_chck () in
    let peak_field         = new Textfield.t ~label:"Peak" ~input_type:(Widget.Float (Some (-100., 100.))) () in
    let cont_en_chck       = new Checkbox.t () in
    let cont_en_field      = new Form_field.t ~label:"Cont enable" ~input:cont_en_chck () in
    let cont_field         = new Textfield.t ~label:"Cont" ~input_type:(Widget.Float (Some (-100., 100.))) () in
    let dur_field          = new Textfield.t ~label:"Duration" ~input_type:(Widget.Float (Some (-100., 100.))) () in
    peak_en_chck#set_checked s.peak_en;
    cont_en_chck#set_checked s.cont_en;
    peak_field#fill_in s.peak;
    cont_field#fill_in s.cont;
    dur_field#fill_in s.duration;
    let peak_box           = new Box.t ~vertical:false
                               ~widgets:[peak_en_field#widget; peak_field#widget;] () in
    let cont_box           = new Box.t ~vertical:false
                               ~widgets:[cont_en_field#widget; cont_field#widget;] () in
    let box                = new Box.t ~widgets:[(Widget.create header);
                                                 peak_box#widget;
                                                 cont_box#widget;
                                                 dur_field#widget] () in
    let signal             = React.S.l5 (fun peak_en peak cont_en cont dur ->
                                 Settings.{ peak_en
                                          ; peak     = CCOpt.get_or ~default:(s.peak) peak
                                          ; cont_en
                                          ; cont     = CCOpt.get_or ~default:(s.cont) cont
                                          ; duration = CCOpt.get_or ~default:(s.duration) dur } )
                               peak_en_chck#s_state peak_field#s_input
                               cont_en_chck#s_state cont_field#s_input
                               dur_field#s_input in
    box, signal
    
  let make_black (b : Settings.black) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Black";
    let black_w, black_s   = make_setting "black" b.black in
    let luma_w, luma_s     = make_setting "luma" b.luma in
    let bpixel_field       = new Textfield.t ~label:"pixel_field" ~input_type:(Widget.Integer (Some (1,256))) () in
    bpixel_field#fill_in b.black_pixel;
    let box                = new Box.t ~widgets:[(Widget.create header);
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
    header##.textContent   := Js.some @@ Js.string "Freeze";
    let freeze_w, freeze_s = make_setting "freeze" f.freeze in
    let diff_w, diff_s     = make_setting "diff" f.diff in
    let pixeld_field       = new Textfield.t ~label:"pixel_diff" ~input_type:(Widget.Integer (Some (1,256))) () in
    pixeld_field#fill_in f.pixel_diff;
    let box                = new Box.t ~widgets:[(Widget.create header);
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
    header##.textContent   := Js.some @@ Js.string "Blocky";
    let blocky_w, blocky_s = make_setting "blocky" b.blocky in
    let mark_chck          = new Checkbox.t () in
    let mark_field         = new Form_field.t ~label:"Mark blocks" ~input:mark_chck () in
    mark_chck#set_checked b.mark_blocks;
    let box                = new Box.t ~widgets:[(Widget.create header);
                                                 blocky_w#widget;
                                                 mark_field#widget] () in
    let signal             = React.S.l2 (fun blocky markb -> Settings.{ blocky; mark_blocks = markb })
                               blocky_s mark_chck#s_state in
    box, signal
    
  let make_video (v : Settings.video) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Video";
    let loss_field         = new Textfield.t ~label:"loss" ~input_type:(Widget.Float (Some (0.,1.))) () in
    loss_field#fill_in v.loss;
    let black_w, black_s   = make_black v.black in
    let freeze_w, freeze_s = make_freeze v.freeze in
    let blocky_w, blocky_s = make_blocky v.blocky in
    let box                = new Box.t ~widgets:[(Widget.create header);
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
    header##.textContent   := Js.some @@ Js.string "Silence";
    let sil_w, sil_s       = make_setting "silence" s.silence in
    let box                = new Box.t ~widgets:[(Widget.create header); sil_w#widget] () in
    let signal             = React.S.map (fun silence -> Settings.{ silence } ) sil_s in
    box, signal

  let make_loudness (s : Settings.loudness) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Loudness";
    let sil_w, sil_s       = make_setting "silence" s.loudness in
    let box                = new Box.t ~widgets:[(Widget.create header); sil_w#widget] () in
    let signal             = React.S.map (fun loudness -> Settings.{ loudness } ) sil_s in
    box, signal

  let make_adv (s : Settings.adv) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Adv";
    let diff               = new Textfield.t ~label:"diff" ~input_type:(Widget.Float (Some (0.,100.))) () in
    let buf                = new Textfield.t ~label:"buf" ~input_type:(Widget.Integer (Some (0,100))) () in
    diff#fill_in s.adv_diff;
    buf#fill_in s.adv_buf;
    let box                = new Box.t ~widgets:[(Widget.create header); diff#widget; buf#widget] () in
    let signal             = React.S.l2 (fun diff buf ->
                                 Settings.{ adv_diff = CCOpt.get_or ~default:(s.adv_diff) diff
                                          ; adv_buf  = CCOpt.get_or ~default:(s.adv_buf) buf })
                               diff#s_input buf#s_input in
    box, signal

  let make_audio (a : Settings.audio) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Audio";
    let loss_field         = new Textfield.t ~label:"loss" ~input_type:(Widget.Float (Some (0.,1.))) () in
    loss_field#fill_in a.loss;
    let silence_w, sil_s   = make_silence  a.silence in
    let loudness_w, loud_s = make_loudness a.loudness in
    let adv_w, adv_s       = make_adv      a.adv in
    let box                = new Box.t ~widgets:[(Widget.create header);
                                                 loss_field#widget;
                                                 silence_w#widget;
                                                 loudness_w#widget;
                                                 adv_w#widget] () in
    let signal             = React.S.l4 (fun loss silence loudness adv ->
                                 match loss with
                                 | None -> Settings.{ loss = a.loss; silence; loudness; adv }
                                 | Some loss -> Settings.{ loss; silence; loudness; adv } )
                               loss_field#s_input sil_s loud_s adv_s in
    box, signal

  let make_layout (s : Settings.t) =
    let header             = Dom_html.createH5 Dom_html.document in
    header##.textContent   := Js.some @@ Js.string "Settings";
    let v, v_s = make_video s.video in
    let a, a_s = make_audio s.audio in
    let s      = React.S.l2 (fun v a -> Settings.{ video = v; audio = a; }) v_s a_s in
    let box    = new Box.t ~widgets:[(Widget.create header); v#widget; a#widget] () in
    box, s
  
  let create
        ~(init:   Settings.t)
        ~(events: Settings.t React.event)
        ~(post:   Settings.t -> unit) =
    let id  = "settings-place" in
    let div = Dom_html.createDiv Dom_html.document in
    let make (set : Settings.t) =
      let dis, s = make_layout set in
      let but    = new Components.Button.t ~label:"send" () in
      let place  = new Components.Card.t
                     ~sections:[`Primary (new Card.Primary.t ~widgets:[dis] ());
                                `Actions (new Card.Actions.t ~widgets:[but] ())]
                     () in
      place#set_id id;
      but#button_element##.onclick := Dom.handler (fun _ -> post @@ React.S.value s; Js._false);
      place
    in
    let _ = React.E.map (fun s ->
                (try Dom.removeChild div (Dom_html.getElementById id)
                 with _ -> print_endline "No el");
                Dom.appendChild div (make s)#root)
              events
    in
    Dom.appendChild div (make init)#root;
    div
end
