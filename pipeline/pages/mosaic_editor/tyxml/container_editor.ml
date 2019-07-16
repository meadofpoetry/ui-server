open Components_tyxml
open Pipeline_types

module CSS = struct
  let root = "container-editor"
  let mode_switch = BEM.add_element root "mode-switch"
  let aspect_ratio_sizer = BEM.add_element root "aspect-ratio-sizer"
  let widget_wrapper = BEM.add_element root "widget-wrapper"
  let widget = BEM.add_element root "widget"
  let content_mode = BEM.add_modifier root "content-mode"
  let cell_dragover = BEM.add_modifier Grid.CSS.cell "dragover"
  let cell_dragging = BEM.add_modifier Grid.CSS.cell "dragging"

  let dialog_add_table = BEM.add_modifier Dialog.CSS.root "add-table"

  let nav_icon_main = BEM.add_modifier Top_app_bar.CSS.navigation_icon "main"
  let nav_icon_aux = BEM.add_modifier Top_app_bar.CSS.navigation_icon "aux"

  let top_app_bar_contextual = BEM.add_modifier Top_app_bar.CSS.root "contextual"
end

let aspect_attr_value (w, h : int * int) =
  Printf.sprintf "%dx%d" w h

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html
  open Utils

  module Widget' = Widget.Make(Xml)(Svg)(Html)

  module Card' = Card.Make(Xml)(Svg)(Html)
  module Tab = Tab.Make(Xml)(Svg)(Html)
  module Tab_indicator = Tab_indicator.Make(Xml)(Svg)(Html)
  module Tab_scroller = Tab_scroller.Make(Xml)(Svg)(Html)
  module Tab_bar = Tab_bar.Make(Xml)(Svg)(Html)

  let create_widget ?(classes = []) ?attrs
      (container : Wm.position)
      (id, widget : string * Wm.widget) : 'a elt =
    let style = Printf.sprintf "z-index: %d" widget.layer in
    let classes = CSS.widget :: classes in
    let parent_size =
      float_of_int @@ container.right - container.left,
      float_of_int @@ container.bottom - container.top in
    div ~a:([ a_class classes
            ; a_style style ]
            @ Widget'.to_html_attributes ~id ~parent_size widget
            <@> attrs)
      []

  let create_widget_wrapper ?(classes = []) ?attrs widgets : 'a elt =
    let classes = CSS.widget_wrapper :: classes in
    div ~a:([a_class classes] <@> attrs) widgets

  let create_mode_switch () =
    let create_tab ?active label =
      let indicator = Tab_indicator.create ?active
          (Tab_indicator.create_content ())
          () in
      let text_label = Tab.create_text_label label () in
      Tab.create ?active ~min_width:true ~indicator
        (Tab.create_content ~text_label ())
        () in
    let tabs =
      [ create_tab ~active:true "Контейнеры"
      ; create_tab "Таблица" ] in
    let scroll_area =
      Tab_scroller.create_scroll_area
        ~content:(Tab_scroller.create_scroll_content tabs ())
        () in
    let scroller = Tab_scroller.create
        ~align:End
        ~scroll_area
        () in
    Tab_bar.create ~classes:[CSS.mode_switch] ~scroller ()

  let create ?(classes = []) ?attrs
      ?(primary_actions = [])
      ~(width : float)
      ~(height : float)
      ~grid
      () : 'a elt =
    let classes = CSS.root :: Card.CSS.root :: Card.CSS.outlined :: classes in
    div ~a:([a_class classes] <@> attrs)
      ([ Card'.create_primary [create_mode_switch ()] ()
       ; Card'.create_media
           [ svg ~a:[ Svg.a_class [CSS.aspect_ratio_sizer]
                    ; Svg.a_viewBox (0., 0., width, height)
                    ] []
           ; grid
           ] ()
       ; Card'.create_actions [] ()
       ])

end
