open Components_tyxml

module CSS = struct
  let root = "container-editor"
  let mode_switch = BEM.add_element root "mode-switch"
  let aspect_ratio_sizer = BEM.add_element root "aspect-ratio-sizer"
  let cell_description = BEM.add_element root "cell-description"
  let content_mode = BEM.add_modifier root "content-mode"
  let cell_dragover = BEM.add_modifier Resizable_grid.CSS.cell "dragover"
  let cell_dragging = BEM.add_modifier Resizable_grid.CSS.cell "dragging"
end

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

  let create_cell_description ?(classes = []) ?attrs ?(content = []) () : 'a elt =
    let classes = CSS.cell_description :: classes in
    span ~a:([a_class classes] <@> attrs) content

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
