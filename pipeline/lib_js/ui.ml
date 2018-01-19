open Components

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

  let make_layout d (wm: Wm.t) =
    let open Layout in
    Layout.initialize d wm

  let create
        ~(init:   Wm.t)
        ~(events: Wm.t React.event)
        ~(post:   Wm.t -> unit) =
    let open Layout in
    let id  = "wm-place" in
    let div = Dom_html.createDiv Dom_html.document in
    let make (wm : Wm.t) =
      let place = Dom_html.createDiv Dom_html.document in
      place##.id := Js.string id;
      let apply  = new Button.t ~label:"apply" () in
      Dom.appendChild place apply#root;
      let layout = make_layout place wm in
      apply#root##.onclick :=
        Dom.handler (fun _ -> post { wm with layout = React.S.value layout }; Js._false);
      place
    in
    let _ = React.E.map (fun s ->
                (try Dom.removeChild div (Dom_html.getElementById id)
                 with _ -> print_endline "No el");
                Dom.appendChild div (make s))
              events
    in
    Dom.appendChild div (make init);
    div
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
(*    Dom.appendChild div (make init)#root;*)
    div
end
