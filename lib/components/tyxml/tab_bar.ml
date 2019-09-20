module CSS = struct
  (** Mandatory. *)
  let root = "mdc-tab-bar"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module Tab_scroller_markup = Tab_scroller.Make (Xml) (Svg) (Html)

  let tab_bar
      ?(classes = return [])
      ?(a = [])
      ?tabs
      ?align
      ?(scroller = Tab_scroller_markup.tab_scroller ?align ?tabs ())
      () =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    div
      ~a:(a_class classes :: a_role (return ["tablist"]) :: a)
      (singleton (return scroller))
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
