open Components_tyxml

module CSS = struct
  include Ui_templates_tyxml.Settings_page.CSS

  let dialog = BEM.add_element section "dialog"

  let empty_placeholder = BEM.add_element section "empty-placeholder"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Floating_label = Floating_label.Make (Xml) (Svg) (Html)
  module Icon_button = Icon_button.Make (Xml) (Svg) (Html)
  module Icon = Icon.Make (Xml) (Svg) (Html)
  module Line_ripple = Line_ripple.Make (Xml) (Svg) (Html)
  module Textfield = Textfield.Make (Xml) (Svg) (Html)
  include Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)

  let create_remove_button () =
    Icon_button.icon_button
      ~classes:[Item_list.CSS.item_meta]
      ~icon:(Icon.SVG.icon ~d:Svg_icons.delete ())
      ()

  let create_textfield ?value ~label ~id () : 'a elt =
    let input_id = id ^ "-input" in
    Textfield.create ?value ~attrs:[a_id id] ~input_id ~label:(`Text label) ()
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
