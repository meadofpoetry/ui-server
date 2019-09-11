open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-service-info"

  let description = BEM.add_modifier root "description"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Item_list_markup = Item_list.Make (Xml) (Svg) (Html)

  let create_description ?(classes = []) ?(attrs = []) () =
    let classes = CSS.description :: classes in
    div ~a:([a_class classes] @ attrs) []
end
