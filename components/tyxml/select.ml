open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-select"
  let native_control_class = CSS.add_element base_class "native-control"
  let is_changing_class = CSS.add_modifier base_class "is-changing"
  let disabled_class = CSS.add_modifier base_class "disabled"

  module Item = struct
    let create ?(classes = []) ?attrs ?(disabled = false)
          ?(selected = false) ~text () : 'a elt =
      option ~a:([a_class classes]
                 |> cons_if disabled @@ a_disabled ()
                 |> cons_if selected @@ a_selected () <@> attrs)
        (txt text)

    let create_group ?(classes = []) ?attrs ~label ~items () : 'a elt =
      optgroup ~a:([a_class classes] <@> attrs) ~label items
  end

  let create_select ?(classes = []) ?attrs
        ?(disabled = false) ~items () : 'a elt =
    select ~a:([a_class (native_control_class::classes)]
               |> cons_if disabled @@ a_disabled ()
               <@> attrs) items

  let create ?(classes=[]) ?attrs ?label ?bottom_line ~select () : 'a elt =
    let opt = label ^:: bottom_line ^:: [] in
    div ~a:([a_class (base_class :: classes)] <@> attrs) (select :: opt)

end
