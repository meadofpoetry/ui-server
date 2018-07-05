open Containers
open Components

let base_class = "mdc-placeholder"

let create_base ~widget ~text () =
  let content_class = Components_markup.CSS.add_element base_class "content"   in
  let ph  = Dom_html.createDiv Dom_html.document |> Widget.create in
  let box = new Box.t ~vertical:true ~widgets:[widget#widget;text#widget] () in
  let ()  = box#add_class content_class in
  let ()  = ph#add_class base_class in
  let ()  = Dom.appendChild ph#root box#root in
  let ()  = widget#add_class @@ Components_markup.CSS.add_element base_class "widget" in
  let ()  = text#add_class @@ Components_markup.CSS.add_element base_class "text" in
  ph

let create_icon ?action ~text ~icon () =
  let _class = Components_markup.CSS.add_modifier base_class "icon" in
  let ico    = match action with
    | Some _ -> let ico = new Icon.Button.Font.t ~icon () in
                let ()  = Option.iter (fun f -> f ico#e_click |> ignore) action in
                ico#widget
    | None   -> let ico = new Icon.Font.t ~icon () in
                ico#widget
  in
  let text = new Typography.Text.t ~split:true ~adjust_margin:false ~text () in
  let ph   = create_base ~widget:ico ~text () in
  let ()   = ph#add_class _class in
  ph

let create_error ?action ?(icon="error") ?(text="error") () =
  let _class = Components_markup.CSS.add_modifier base_class "error" in
  let ph     = create_icon ?action ~icon ~text () in
  let ()     = ph#add_class _class in
  ph

let create_progress ?(text="Загрузка") () =
  let _class = Components_markup.CSS.add_modifier base_class "progress" in
  let make_dot () = let dot = Dom_html.createSpan Dom_html.document |> Widget.create in
                    dot#set_text_content ".";
                    dot
  in
  let w  = new Circular_progress.t ~indeterminate:true () in
  let p  = Dom_html.createP Dom_html.document |> Widget.create in
  let () = p#set_text_content text in
  let () = List.iter (fun _ -> Dom.appendChild p#root (make_dot ())#root) @@ List.range' 0 3 in
  let ph = create_base ~widget:w ~text:p () in
  let () = ph#add_class _class in
  ph
