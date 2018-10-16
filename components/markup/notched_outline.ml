open Utils
open Containers

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-notched-outline"
  let notched_class = CSS.add_modifier base_class "notched"
  let path_class = CSS.add_element base_class "path"
  let idle_class = CSS.add_element base_class "idle"

  let create_path ?(classes = []) ?attrs () =
    Svg.path ~a:([Svg.a_class (path_class :: classes)] <@> attrs) []

  let create_svg ?(classes = []) ?attrs path () =
    svg ~a:([Svg.a_class classes] <@> attrs) [path]

  let create_idle ?(classes = []) ?attrs () =
    div ~a:([a_class (idle_class :: classes)] <@> attrs) []

  let create ?(classes = []) ?attrs svg () =
    div ~a:([ a_class (base_class :: classes) ] <@> attrs) [svg]

end
