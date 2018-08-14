open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  open Html

  let base_class   = "mdc-fab"
  let icon_class   = CSS.add_element base_class "icon"
  let exited_class = CSS.add_modifier base_class "exited"
  let mini_class   = CSS.add_modifier base_class "mini"

  let create ?(classes=[]) ?attrs ?(mini=false) ?label ~icon () =
    button ~a:([ a_class (classes
                          |> cons_if mini @@ mini_class
                          |> List.cons base_class
                          |> List.cons "material-icons") ]
               |> map_cons_option (fun x -> a_aria "label" [x]) label
               <@> attrs)
      [ icon ]

end
