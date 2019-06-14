module CSS = struct
  include Side_sheet.Make_css(struct let root = "mdc-drawer" end)

  (** Non-scrollable element that exists at the top of the drawer. *)
  let header = BEM.add_element root "header"

  (** Title text element of the drawer. *)
  let title = BEM.add_element root "title"

  (** Subtitle text element of the drawer. *)
  let subtitle = BEM.add_element root "subtitle"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create_scrim ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.scrim :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create_title ?(tag = h3) ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.title :: classes in
    tag ~a:([a_class classes] <@> attrs) content

  let create_subtitle ?(tag = h6) ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.subtitle :: classes in
    tag ~a:([a_class classes] <@> attrs) content

  let create_header ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.header :: classes in
    div ~a:([a_class classes] <@> attrs) content

  let create_content ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.content :: classes in
    div ~a:([a_class classes] <@> attrs) content

  let create ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.root :: classes in
    aside ~a:([a_class classes] <@> attrs) content
end
