open Containers
open Components

let base_class = "mdc-placeholder"

let create_base ~widget ~text () =
  let content_class = Components_markup.CSS.add_element base_class "content" in
  let ph = Widget.create_div () in
  let box = new Vbox.t ~widgets:[ widget#widget; text#widget ] () in
  box#add_class content_class;
  ph#add_class base_class;
  ph#append_child box;
  widget#add_class @@ Components_markup.CSS.add_element base_class "widget";
  text#add_class @@ Components_markup.CSS.add_element base_class "text";
  ph

let create_with_icon ?action ~text ~icon () =
  let _class = Components_markup.CSS.add_modifier base_class "icon" in
  let ico = match action with
    | Some f ->
       let btn = new Icon_button.t ~icon () in
       btn#listen_lwt Widget.Event.click (fun e _ ->
           f e; Lwt.return_unit) |> Lwt.ignore_result;
       btn#widget
    | None -> icon#widget
  in
  let text = new Typography.Text.t ~split:true ~adjust_margin:false ~text () in
  let ph = create_base ~widget:ico ~text () in
  let () = ph#add_class _class in
  ph

let create_with_error ?action ?icon ?(text="error") () =
  let icon = match icon with
    | Some icon -> icon
    | None -> Icon.SVG.(create_simple Path.alert_decagram) in
  let _class = Components_markup.CSS.add_modifier base_class "error" in
  let ph = create_with_icon ?action ~icon ~text () in
  let () = ph#add_class _class in
  ph

let create_progress ?(text="Загрузка") () =
  let _class = Components_markup.CSS.add_modifier base_class "progress" in
  let make_dot () =
    let dot = Widget.create_span () in
    dot#set_text_content ".";
    dot in
  let w = new Circular_progress.t ~indeterminate:true () in
  let p = Dom_html.createP Dom_html.document |> Widget.create in
  let () = p#set_text_content text in
  let () = List.iter (fun _ -> p#append_child (make_dot ()))
           @@ List.range' 0 3 in
  let ph = create_base ~widget:w ~text:p () in
  let () = ph#add_class _class in
  ph

let under_development () =
  let icon = Icon.SVG.(create_simple Path.crane) in
  let text = new Typography.Text.t
               ~text:"Страница находится в разработке" () in
  let ph = create_base ~widget:icon ~text () in
  let _class = Components_markup.CSS.add_modifier base_class "icon" in
  ph#add_class _class;
  ph
