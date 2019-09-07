module CSS = struct
  (** Mandatory. *)
  let root = "mdc-tab-bar"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Tab_scroller_markup = Tab_scroller.Make (Xml) (Svg) (Html)

  let create
      ?(classes = [])
      ?(attrs = [])
      ?tabs
      ?align
      ?(scroller = Tab_scroller_markup.create ?align ?tabs ())
      () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes; a_role ["tablist"]] @ attrs) [scroller]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
