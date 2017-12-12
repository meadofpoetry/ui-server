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
    print_endline (Yojson.Safe.pretty_to_string @@ Structure.t_list_to_yojson init);
    let make (str : Structure.t list) =
      let place  = Dom_html.createDiv Dom_html.document in
      place##.id := Js.string id;
      let but    = new Components.Button.t ~label:"send" () in
      let dis, s = make_structure_list str in
      Dom.appendChild place dis#root;
      Dom.appendChild place but#root;
      but#root##.onclick := Dom.handler (fun _ -> post @@ React.S.value s; Js._false);
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

module Wm = struct
  
  let make_layout div (wdgl : (string * Wm.widget) list) =
    let open Layout in
    List.map (fun (n,_) ->
        let e = Dom_html.createDiv Dom_html.document in
        Layout.Style.z_index e n;
        Layout.Style.class_name e "doge";
        e)
      wdgl
    |> List.fold_left (fun acc e -> Layout.add_element e div acc 1 1 1 1 false) []
    |> List.rev
  
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
      let plane  = Dom_html.createDiv Dom_html.document in
      Layout.Style.class_name plane "div";
      Layout.Style.width plane (hor*cols-5);
      Layout.Style.height plane (vert*lines-5);
      Dom.appendChild place plane;
      Dom.appendChild place apply#root;
      let lst = make_layout plane wm.widgets in
      apply#root##.onclick :=
        Dom.handler (fun _ ->
            let layout = CCList.map2 (fun (name, widg) (e : Layout.Element.t) ->
                             let (position : Wm.position) =
                               { left = e.x * 100; right = (e.x + e.width) * 100; top = e.y * 100; bottom = (e.y + e.height) * 100 }
                             in
                             (name,
                              { position = position
                              ; widgets  =
                                  [ (name, { widg with position = position }) ] } : (string * Wm.container) ))
                           wm.widgets lst
            in post { wm with layout }; Js._false);
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

          (*
module Settings = struct

  let make_video (v : Settings.video) =
    let loss_s, push = React.S.create v.loss in
    let div    = Dom_html.createDiv Dom_html.document in
    let lfield = Components.Textfield.create ~input_type:(`Number) () in
    let vlabel = Dom_html.createLabel Dom_html.document in

  let make_layout (s : Settings.t) =
    let v, vs = make_video s.video in
    let a, as = make_audio s.audio in
    let s     = React.S.l2 (fun v a -> Settings.{ video = v; audio = a; }) vs as in
    let div    = Dom_html.createDiv Dom_html.document in
    let vlabel = Dom_html.createLabel Dom_html.document in
    let alabel = Dom_html.createLabel Dom_html.document in
    vlabel##.textContent := Js.some @@ Js.string "Video:";
    alabel##.textContent := Js.some @@ Js.string "Audio:";
    Dom.appendChild div vlabel;
    Dom.appendChild div v;
    Dom.appendChild div alabel;
    Dom.appendChild div a;
    div, s
  
  let create
        ~(init:   Settings.t)
        ~(events: Settings.t React.event)
        ~(post:   Settings.t -> unit) =
    let id  = "settings-place" in
    let div = Dom_html.createDiv Dom_html.document in
    let make (set : Settings.t) =
      let place  = Dom_html.createDiv Dom_html.document in
      place##.id := Js.string id;
      let but    = Components.Button.attach @@ Components.Button.create ~label:"send" ~raised:true () in
      let dis, s = make_layout set in
      Dom.appendChild place dis;
      Dom.appendChild place but;
      but##.onclick := Dom.handler (fun _ -> post @@ React.S.value s; Js._false);
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
           *)
