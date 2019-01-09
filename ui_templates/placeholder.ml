open Js_of_ocaml
open Containers
open Components

let base_class = "mdc-placeholder"
let content_class = Markup.CSS.add_element base_class "content"
let widget_class = Markup.CSS.add_element base_class "widget"
let text_class = Markup.CSS.add_element base_class "text"
let with_icon_class = Markup.CSS.add_modifier base_class "icon"
let with_progress_class = Markup.CSS.add_modifier base_class "progress"
let with_error_class = Markup.CSS.add_modifier base_class "error"

module Base = struct
  class t ~widget ~text () =
    let box = new Vbox.t ~widgets:[widget#widget; text#widget] () in
    object
      inherit Widget.t Dom_html.(createDiv document) () as super
      method! init () : unit =
        super#init ();
        widget#add_class widget_class;
        text#add_class text_class;
        box#add_class content_class;
        super#add_class base_class;
        super#append_child box
    end
end

module With_icon = struct
  class ['a] t ?action ~text ~(icon  : (#Widget.t) as 'a) () =
    let ico = match action with
      | Some f ->
         let btn = new Icon_button.t ~icon () in
         btn#listen_lwt Widget.Event.click (fun e _ ->
             f e; Lwt.return_unit) |> Lwt.ignore_result;
         btn#widget
      | None -> icon#widget in
    let text =
      new Typography.Text.t
        ~split:true
        ~adjust_margin:false
        ~text () in
    object
      inherit Base.t ~widget:ico ~text () as super
      method! init () : unit =
        super#init ();
        super#add_class with_icon_class
      method text_widget = text
      method icon : 'a = icon
      method set_text (s : string) : unit =
        text#set_text s
    end
end

module With_progress = struct

  let make_dot () =
    let dot = Widget.create_span () in
    dot#set_text_content ".";
    dot

  let create_text text =
    let w = Widget.create @@ Dom_html.(createP document) in
    w#set_text_content text;
    List.iter (fun _ -> w#append_child @@ make_dot ())
    @@ List.range' 0 3;
    w

  class t ?(indeterminate = true) ?(text = "Загрузка") () =
    let w = new Circular_progress.t ~indeterminate () in
    let p = create_text text in
    object
      inherit Base.t ~widget:w ~text:p () as super
      method! init () : unit =
        super#init ();
        super#add_class with_progress_class
      method progress = w
    end
end

let create_with_icon ?action ~text ~icon () =
  new With_icon.t ?action ~text ~icon ()

let create_progress ?indeterminate ?text () =
  new With_progress.t ?indeterminate ?text ()

let error_svg_path = Icon.SVG.Path.alert_decagram

let create_error_icon () =
  Icon.SVG.(create_simple error_svg_path)

let create_with_error ?action ?icon ?(text = "error") () =
  let icon = match icon with
    | Some icon -> icon
    | None -> create_error_icon () in
  let ph = create_with_icon ?action ~icon ~text () in
  ph#add_class with_error_class;
  ph

let under_development () =
  let icon = Icon.SVG.(create_simple Path.crane) in
  let text =
    new Typography.Text.t
      ~text:"Страница находится в разработке" () in
  let ph =
    new Base.t ~widget:icon ~text () in
  ph#add_class with_icon_class;
  ph
