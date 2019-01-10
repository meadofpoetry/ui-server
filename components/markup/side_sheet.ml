open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module CSS = struct
    include CSS
    let base = "mdc-side-sheet"
    let dismissible = add_modifier base "dismissible"
    let modal = add_modifier base "modal"
    let open_ = add_modifier base "open"
    let opening = add_modifier base "opening"
    let closing = add_modifier base "closing"
    let animate = add_modifier base "animate"
    let content = add_element base "content"

    let scrim = base ^ "-scrim"
    let app_content = base ^ "-app-content"
    let scroll_lock = base ^ "-scroll-lock"
  end

  let create_scrim ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.scrim :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create_content ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.content :: classes in
    div ~a:([a_class classes] <@> attrs) content

  let create ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.base :: classes in
    aside ~a:([a_class classes] <@> attrs) content

end
