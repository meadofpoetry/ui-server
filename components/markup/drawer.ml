open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-drawer"
  let temporary_class = CSS.add_modifier base_class "temporary"
  let drawer_class = CSS.add_element base_class "drawer"
  let content_class = CSS.add_element base_class "content"
  let open_class = CSS.add_modifier base_class "open"
  let animating_class = CSS.add_modifier base_class "animating"
  let scroll_lock_class = base_class ^ "-scroll-lock"

  let anchor_left_class = CSS.add_modifier drawer_class "anchor-left"
  let anchor_right_class = CSS.add_modifier drawer_class "anchor-right"
  let anchor_top_class = CSS.add_modifier drawer_class "anchor-top"
  let anchor_bottom_class = CSS.add_modifier drawer_class "anchor-bottom"

  module Toolbar_spacer = struct

    let _class = CSS.add_element base_class "toolbar-spacer"

    let create ?(classes = []) ?attrs ~content () : 'a elt =
      div ~a:([ a_class (_class :: classes) ] <@> attrs) content

  end

  module Header = struct

    let _class = CSS.add_element base_class "header"
    let header_content_class = CSS.add_element base_class "header-content"

    let create ?(classes = []) ?attrs ~content () : 'a elt =
      header ~a:([a_class (_class :: classes)] <@> attrs)
        [div ~a:[a_class [header_content_class]] content]

  end

  let create_drawer ?(classes = []) ?attrs ~content () : 'a elt =
    nav ~a:([a_class (drawer_class :: classes)] <@> attrs) content

  let create ?(classes = []) ?attrs ~drawer () : 'a elt =
    aside ~a:([a_class (base_class :: temporary_class :: classes)]
              <@> attrs)
      [drawer]

end
