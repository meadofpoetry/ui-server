module CSS = struct
  include Side_sheet.Make_css (struct
    let root = "mdc-drawer"
  end)

  (** Non-scrollable element that exists at the top of the drawer. *)
  let header = BEM.add_element root "header"

  (** Title text element of the drawer. *)
  let title = BEM.add_element root "title"

  (** Subtitle text element of the drawer. *)
  let subtitle = BEM.add_element root "subtitle"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_scrim ?(classes = []) ?(attrs = []) ?(children = []) () : 'a elt =
    let classes = CSS.scrim :: classes in
    div ~a:([a_class classes] @ attrs) children

  let create_title ?(tag = h3) ?(classes = []) ?(attrs = []) ?title ?(children = []) () =
    let classes = CSS.title :: classes in
    tag ~a:([a_class classes] @ attrs) (Utils.map_cons_option txt title children)

  let create_subtitle
      ?(tag = h6)
      ?(classes = [])
      ?(attrs = [])
      ?subtitle
      ?(children = [])
      () =
    let classes = CSS.subtitle :: classes in
    tag ~a:([a_class classes] @ attrs) (Utils.map_cons_option txt subtitle children)

  let create_header ?(classes = []) ?(attrs = []) ?(children = []) () : 'a elt =
    let classes = CSS.header :: classes in
    div ~a:([a_class classes] @ attrs) children

  let create_content ?(classes = []) ?(attrs = []) ?(children = []) () : 'a elt =
    let classes = CSS.content :: classes in
    div ~a:([a_class classes] @ attrs) children

  let create ?(classes = []) ?(attrs = []) ?(children = []) () =
    let classes = CSS.root :: classes in
    aside ~a:([a_class classes] @ attrs) children
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
