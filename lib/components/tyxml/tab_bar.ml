module CSS = struct
  (** Mandatory. *)
  let root = "mdc-tab-bar"
end

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module CSS = CSS
  module Tab_scroller = Tab_scroller.Make (Xml) (Svg) (Html)

  let tab_bar
      ?(classes = [])
      ?(a = [])
      ?tabs
      ?align
      ?(scroller = Tab_scroller.tab_scroller ?align ?tabs ())
      () =
    let classes = CSS.root :: classes in
    div
      ~a:([a_class (return classes); a_role (return ["tablist"])] @ a)
      (singleton (return scroller))
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
