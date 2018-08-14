open Utils
open Containers

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  open Html

  let base_class = "mdc-button"
  let icon_class = CSS.add_element base_class "icon"
  let text_class = CSS.add_element base_class "text"

  let unelevated_class = CSS.add_modifier base_class "unelevated"
  let stroked_class    = CSS.add_modifier base_class "stroked"
  let raised_class     = CSS.add_modifier base_class "raised"

  let dense_class      = CSS.add_modifier base_class "dense"
  let compact_class    = CSS.add_modifier base_class "compact"

  let create ?(classes=[]) ?attrs ?button_type ?button_style
        ?(disabled=false) ?(dense=false) ?(compact=false)
        ?icon ?label () =
    button ~a:([ a_class (classes
                          |> map_cons_option (function
                                 | `Raised     -> raised_class
                                 | `Stroked    -> stroked_class
                                 | `Unelevated -> unelevated_class) button_style
                          |> cons_if dense      dense_class
                          |> cons_if compact    compact_class
                          |> List.cons base_class) ]
               |> map_cons_option a_button_type button_type
               |> cons_if disabled @@ a_disabled ()
               <@> attrs)
      ([]
       |> map_cons_option (fun x -> span ~a:[a_class [ text_class ]]
                                      [ pcdata x]) label
       |> cons_option icon)

end
