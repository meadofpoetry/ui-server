open Components_tyxml

module CSS = struct
  let root = "mdc-overflow-menu"

  let actions = BEM.add_element root "actions"

  let overflow = BEM.add_element root "overflow"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Icon = Icon.Make (Xml) (Svg) (Html)
  module Icon_button = Icon_button.Make (Xml) (Svg) (Html)

  let create_overflow ?(classes = []) ?attrs ?icon () =
    let classes = Top_app_bar.CSS.action_item :: classes in
    let icon =
      match icon with
      | Some x -> x
      | None -> Icon.SVG.(create_of_d Svg_icons.dots_vertical)
    in
    Icon_button.create ~classes ?attrs ~icon ()

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(overflow = create_overflow ())
      ?menu
      ~actions
      () : 'a elt =
    let classes = CSS.root :: classes in
    div
      ~a:([a_class classes] @ attrs)
      [ div ~a:[a_class [CSS.actions]] actions
      ; div ~a:[a_class [CSS.overflow]] Utils.(overflow :: (menu ^:: [])) ]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
