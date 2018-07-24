open Utils
open Containers

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class       = "mdc-box"
  let vertical_class   = CSS.add_modifier base_class "vertical"
  let horizontal_class = CSS.add_modifier base_class "horizontal"

  let justify_content_class_prefix = CSS.add_modifier base_class "justify-content-"
  let align_items_class_prefix     = CSS.add_modifier base_class "align-items-"
  let align_content_class_prefix   = CSS.add_modifier base_class "align-content-"

  let get_wrap_class x =
    CSS.add_modifier base_class
      (match x with
       | `Nowrap       -> "nowrap"
       | `Wrap         -> "wrap"
       | `Wrap_reverse -> "wrap-reverse")

  let get_justify_content_class x =
    justify_content_class_prefix
    ^ (match x with
       | `Start         -> "start"
       | `End           -> "end"
       | `Center        -> "center"
       | `Space_between -> "space-between"
       | `Space_around  -> "space-around"
       | `Space_evenly  -> "space-evenly")

  let get_align_items_class x =
    align_items_class_prefix
    ^ (match x with
       | `Start    -> "start"
       | `End      -> "end"
       | `Center   -> "center"
       | `Stretch  -> "stretch"
       | `Baseline -> "baseline")

  let get_align_content_class x =
    align_content_class_prefix
    ^ (match x with
       | `Start         -> "start"
       | `End           -> "end"
       | `Center        -> "center"
       | `Stretch       -> "stretch"
       | `Space_between -> "space-between"
       | `Space_around  -> "space-around")

  let create ?(classes=[]) ?attrs ?tag
        ?justify_content ?align_items ?align_content ?(vertical=false) ~content () =
    let tag = Option.get_or ~default:div tag in
    tag ~a:([ a_class (classes
                       |> cons_if vertical vertical_class
                       |> map_cons_option get_justify_content_class justify_content
                       |> map_cons_option get_align_items_class align_items
                       |> map_cons_option get_align_content_class align_content
                       |> List.cons base_class) ] <@> attrs)
      content

end
