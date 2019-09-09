open Components_tyxml
open Pipeline_types

module CSS = struct
  let root = "container-editor"

  let mode_switch = BEM.add_element root "mode-switch"

  let mode_switch_hidden = BEM.add_modifier mode_switch "hidden"

  let widget_wrapper = BEM.add_element root "widget-wrapper"

  let widget = BEM.add_element root "widget"

  let cell_dragover = BEM.add_modifier Grid.CSS.cell "dragover"

  let cell_dragging = BEM.add_modifier Grid.CSS.cell "dragging"

  let widget_mode = "widget-mode"

  let dialog_add_table = BEM.add_modifier Dialog.CSS.root "add-table"

  let nav_icon_main = BEM.add_modifier Top_app_bar.CSS.navigation_icon "main"

  let nav_icon_aux = BEM.add_modifier Top_app_bar.CSS.navigation_icon "aux"

  let top_app_bar_contextual = BEM.add_modifier Top_app_bar.CSS.root "contextual"
end

let aspect_attr_value ((w, h) : int * int) = Printf.sprintf "%dx%d" w h

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Widget_markup = Widget.Make (Xml) (Svg) (Html)
  module Card_markup = Card.Make (Xml) (Svg) (Html)

  let create_widget
      ?(classes = [])
      ?(attrs = [])
      ((id, _, widget) : string * Wm.Annotated.state * Wm.widget) : 'a elt =
    let position =
      match widget.position with
      | None -> ""
      | Some pos ->
          Printf.sprintf
            "left: %g%%;top: %g%%;width: %g%%;height: %g%%;"
            (pos.x *. 100.)
            (pos.y *. 100.)
            (pos.w *. 100.)
            (pos.h *. 100.)
    in
    let style = Printf.sprintf "%sz-index: %d" position widget.layer in
    let classes = CSS.widget :: classes in
    div
      ~a:
        ([a_class classes; a_style style]
        @ Widget_markup.to_html_attributes ~id widget
        @ attrs)
      []

  let create_widget_wrapper ?(classes = []) ?(attrs = []) widgets : 'a elt =
    let classes = CSS.widget_wrapper :: classes in
    div ~a:([a_class classes] @ attrs) widgets

  let create ?(classes = []) ?attrs grid : 'a elt =
    Card_markup.create
      ~classes:(CSS.root :: classes)
      ?attrs
      ~children:
        [ Card_markup.create_media ~children:[grid] ()
        ; Card_markup.create_actions ~children:[] () ]
      ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
