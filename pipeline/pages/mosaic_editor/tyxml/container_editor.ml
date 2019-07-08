open Components_tyxml
open Pipeline_types

module CSS = struct
  let root = "container-editor"
  let mode_switch = BEM.add_element root "mode-switch"
  let aspect_ratio_sizer = BEM.add_element root "aspect-ratio-sizer"
  let widget = BEM.add_element root "widget"
  let content_mode = BEM.add_modifier root "content-mode"
  let cell_dragover = BEM.add_modifier Resizable_grid.CSS.cell "dragover"
  let cell_dragging = BEM.add_modifier Resizable_grid.CSS.cell "dragging"
end

let widget_type_to_string = function
  | Wm.Audio -> "audio"
  | Video -> "video"

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html
  open Utils

  module Card' = Card.Make(Xml)(Svg)(Html)
  module Tab = Tab.Make(Xml)(Svg)(Html)
  module Tab_indicator = Tab_indicator.Make(Xml)(Svg)(Html)
  module Tab_scroller = Tab_scroller.Make(Xml)(Svg)(Html)
  module Tab_bar = Tab_bar.Make(Xml)(Svg)(Html)

  let create_widget ?(classes = []) ?attrs
      (id, widget : string * Wm.widget) : 'a elt =
    let domain = Yojson.Safe.to_string @@ Wm.domain_to_yojson widget.domain in
    let aspect = match widget.aspect with
      | None -> None
      | Some (w, h) -> Some (Printf.sprintf "%dx%d" w h) in
    let style = Printf.sprintf "z-index: %d" widget.layer in
    let classes = CSS.widget :: classes in
    div ~a:([ a_id id
            ; a_class classes
            ; a_style style
            ; a_user_data "type" (widget_type_to_string widget.type_)
            ; a_user_data "domain" domain
            ; a_user_data "description" widget.description ] <@> attrs
            |> map_cons_option (a_user_data "aspect") aspect
            |> map_cons_option (a_user_data "pid" % string_of_int) widget.pid)
      []

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
      ~(width : int)
      ~(height : int)
      ~grid
      () : 'a elt =
    let classes = CSS.root :: Card.CSS.root :: Card.CSS.outlined :: classes in
    div ~a:([a_class classes] <@> attrs)
      ([ Card'.create_primary [create_mode_switch ()] ()
       ; Card'.create_media
           [ svg ~a:[ Svg.a_class [CSS.aspect_ratio_sizer]
                    ; Svg.a_viewBox ( 0.
                                    , 0.
                                    , (float_of_int width)
                                    , (float_of_int height)
                                    )
                    ] []
           ; grid
           ] ()
       ; Card'.create_actions [] ()
       ])

end
