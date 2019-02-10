open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  open Html

  module CSS = struct

    let root = "mdc-icon-button"
    let icon = CSS.add_element root "icon"
    let on = CSS.add_modifier root "on"
    let icon_on = CSS.add_modifier icon "on"

  end

  let create ?(classes = []) ?attrs ?(ripple = true)
        ?(on = false) ?(disabled = false) ?on_icon ~icon
        () : 'a elt =
    let classes =
      classes
      |> cons_if on CSS.on
      |> List.cons CSS.root in
    button ~a:([a_class classes]
               |> cons_if_lazy ripple (fun () -> a_user_data "ripple" "true")
               |> cons_if_lazy disabled a_disabled
               <@> attrs)
      (on_icon ^:: icon :: [])

end
