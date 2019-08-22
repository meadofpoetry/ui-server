open Js_of_ocaml
open Components

module CSS = struct
  open Components_tyxml
  let root = "mdc-placeholder"
  let content = BEM.add_element root "content"
  let widget = BEM.add_element root "widget"
  let text = BEM.add_element root "text"

  let icon = BEM.add_modifier root "icon"
  let progress = BEM.add_modifier root "progress"
  let error = BEM.add_modifier root "error"
end

let error_svg_path = Components_tyxml.Svg_icons.alert_decagram

module Base = struct

  class t ~widget ~text () =
    let box = Box.make
        ~align_items:`Center
        ~dir:`Column
        [widget#widget; text#widget] in
    object
      inherit Widget.t Dom_html.(createDiv document) () as super

      method! init () : unit =
        super#init ();
        widget#add_class CSS.widget;
        text#add_class CSS.text;
        box#add_class CSS.content;
        super#add_class CSS.root;
        super#append_child box
    end
end

module With_icon = struct

  class ['a] t ?font ?action ?(text = "") ~(icon : Dom_html.element Js.t) () =
    let ico = match action with
      | Some f -> Widget.coerce @@ Icon_button.make ~icon ~on_click:f ()
      | None -> Widget.create icon in
    let text = Typography.Text.make ?font text in
    object(self)
      inherit Base.t ~widget:ico ~text () as super

      method! init () : unit =
        super#init ();
        super#add_class CSS.icon;
        self#set_text_visibility ()

      method text_widget = text

      method icon : 'a = icon

      method set_text (s : string) : unit =
        text#set_text s;
        self#set_text_visibility ()

      (* Private methods *)

      method private set_text_visibility () : unit =
        match text#text with
        | "" -> text#root##.style##.display := Js.string "none"
        | _ -> text#root##.style##.display := Js.string ""

    end

  let make ?font ?action ~text ~icon () =
    new t ?font ?action ~text ~icon ()

end

module Progress = struct

  let make_dot () =
    let dot = Widget.create_span () in
    dot#root##.textContent := Js.some @@ Js.string ".";
    dot

  let create_text text =
    let w = Widget.create @@ Dom_html.(createP document) in
    w#root##.textContent := Js.some @@ Js.string text;
    for _ = 0 to 2 do w#append_child @@ make_dot () done;
    w

  class t ?(indeterminate = true) ?size ?(text = "Загрузка") () =
    let w = Circular_progress.make ?size ~indeterminate () in
    let p = create_text text in
    object
      inherit Base.t ~widget:w ~text:p () as super

      method! init () : unit =
        super#init ();
        super#add_class CSS.progress

      method progress = w
    end

  let make ?indeterminate ?size ?text () =
    new t ?indeterminate ?size ?text ()

end

module Err = struct

  let create_error_icon () =
    Icon.SVG.(make_simple error_svg_path)#root

  let make ?action ?icon ?(text = "error") () =
    let icon = match icon with
      | Some icon -> icon
      | None -> create_error_icon () in
    let ph = With_icon.make ?action ~icon ~text () in
    ph#add_class CSS.error;
    ph

end

let make_under_development () =
  let icon = Icon.SVG.(make_simple Path.crane) in
  let text = Typography.Text.make "Страница находится в разработке" in
  let ph = new Base.t ~widget:icon ~text () in
  ph#add_class CSS.icon;
  ph
