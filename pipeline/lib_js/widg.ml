open Components

module WStructure = struct
  
  let make_pid (pid : Structure.pid) =
    let text, stext =
      match pid.content with
      | Empty   -> "Empty " ^ pid.stream_type_name, ""
      | Audio a -> "Audio " ^ pid.stream_type_name, Printf.sprintf "Codec: %s; Bitrate: %s;" a.codec a.bitrate
      | Video v -> "Video " ^ pid.stream_type_name, Printf.sprintf "Codec: %s; Resolution: %dx%d;"
                                                      v.codec (fst v.resolution) (snd v.resolution)
    in
    let initial_state  = pid.to_be_analyzed in
    let checkbox       = Dom_html.createInput ~_type:(Js.string "checkbox") Dom_html.document in
    checkbox##.checked := Js.bool initial_state;
    let s, push        = React.S.create initial_state in
    let pid_s          = React.S.map (fun b -> {pid with to_be_analyzed = b}) s in
    checkbox##.onchange := Dom.handler (fun _ -> let b = React.S.value s in push @@ not b; Js._true); 
    List_.Item.create ~text ~secondary_text:stext ~end_detail:(of_dom checkbox) (), pid_s

  let make_channel (ch : Structure.channel) =
    let text, stext =
      Printf.sprintf "Channel %d" ch.number,
      Printf.sprintf "Serv: %s; Provider: %s" ch.service_name ch.provider_name
    in
    let wl, sl = CCList.split @@ CCList.map make_pid ch.pids in
    let ch_s   = React.S.map (fun pl -> {ch with pids = pl}) @@ React.S.merge ~eq:(==) (fun a p -> p::a) [] sl in
    let lst = List_.create ~items:wl ~two_line:true ~style:"max-width: 400px;" () in
    let e = Tyxml_js.Html.div [ List_.Item.create
                                  ~text
                                  ~secondary_text:stext
                                  ~auto_init:true
                                  ()
                              ; lst ]
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
    let lst    = List_.create ~items:wl ~two_line:true ~style:"max-width: 400px;" () in
    let e      = Tyxml_js.Html.div [ List_.Item.create
                                       ~text
                                       ~secondary_text:stext
                                       ~auto_init:true
                                       ()
                                   ; lst ]
    in e, st_s

  let make_structure_list (sl : Structure.t list) =
    let wl, sl = CCList.split @@ CCList.map make_structure sl in
    let sl_s   = React.S.merge ~eq:(==) (fun a p -> p::a) [] sl in
    let lst    = List_.create ~items:wl ~two_line:true ~style:"max-width: 400px;" () in
    List_.attach lst, sl_s


  let id = "structure-widget"
  type t = { div : Dom_html.divElement Js.t
           ; but : Button.t Js.t
           ; dis : Dom_html.divElement Js.t
           }

  let create (str:Structure.t list) post =
    let div = Dom_html.createDiv Dom_html.document in
    div##.id := Js.string id;
    let but = Components.Button.attach @@ Components.Button.create ~label:"post" ~raised:true () in
    let dis, s = make_structure_list str in
    Dom.appendChild div dis;
    Dom.appendChild div but;
    but##.onclick := Dom.handler (fun _ -> post @@ React.S.value s; Js._false);
    { div; but; dis }
    
end
