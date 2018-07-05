open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class           = "mdc-select"
  let native_control_class = CSS.add_element base_class "native-control"
  let is_changing_class    = CSS.add_modifier base_class "is-changing"
  let disabled_class       = CSS.add_modifier base_class "disabled"

  module Item = struct
    let create ?(classes=[]) ?attrs ?(disabled=false) ?(selected=false) ~text () =
      option ~a:([ a_class classes ]
                 |> cons_if disabled @@ a_disabled ()
                 |> cons_if selected @@ a_selected () <@> attrs)
        (pcdata text)

    let create_group ?(classes=[]) ?attrs ~label ~items () =
      optgroup ~a:([ a_class classes ] <@> attrs) ~label items
  end

  let create_select ?(classes=[]) ?attrs ?(disabled=false) ~items () =
    select ~a:([ a_class (native_control_class::classes) ]
               |> cons_if disabled @@ a_disabled ()
               <@> attrs) items

  module Label = struct
    let _class            = CSS.add_element base_class "label"
    let float_above_class = CSS.add_modifier _class "float-above"

    let create ?(classes=[]) ?attrs ~label () =
      div ~a:([ a_class (_class :: classes) ] <@> attrs) [ pcdata label ]
  end

  module Bottom_line = struct
    let _class       = CSS.add_element base_class "bottom-line"
    let active_class = CSS.add_modifier _class "active"

    let create ?(classes=[]) ?attrs () =
      div ~a:([ a_class (_class :: classes ) ] <@> attrs) []
  end

  let create ?(classes=[]) ?attrs ~select ~label ~bottom_line () =
    div ~a:([ a_class (base_class :: classes)] <@> attrs) [ select; label; bottom_line ]

end
