open Utils
open Containers

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-tab-indicator"
  let content_class = CSS.add_element base_class "content"
  let active_class = CSS.add_modifier base_class "active"
  let fade_class = CSS.add_modifier base_class "fade"
  let content_underline_class = CSS.add_modifier content_class "underline"
  let content_icon_class = CSS.add_modifier content_class "icon"

  let create_content ?(classes = []) ?attrs ?(icon = false) () : 'a elt =
    let (classes : string list) =
      classes
      |> cons_if icon content_icon_class
      |> cons_if (not icon) content_underline_class
      |> List.cons content_class in
    span ~a:([a_class classes] <@> attrs) []

  let create ?(classes = []) ?attrs ?(active = false)
        ?(fade = false) content () : 'a elt =
    let (classes : string list) =
      classes
      |> cons_if fade fade_class
      |> cons_if active active_class
      |> List.cons base_class in
    span ~a:([a_class classes] <@> attrs) [content]

end
