module CSS = struct
  (** Mandatory. *)
  let root = "mdc-icon-button"

  (** This class is applied to each icon element for the icon button toggle. *)
  let icon = BEM.add_element root "icon"

  (** This class is applied to the root element and is used to indicate if the
      icon button toggle is in the "on" state. *)
  let on = BEM.add_modifier root "on"

  (** This class is applied to a icon element and is used to indicate
      the toggle button icon that represents the "on" icon. *)
  let icon_on = BEM.add_modifier icon "on"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

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

  let create_anchor ?(classes = []) ?attrs ?href ?(ripple = true)
      ?(on = false) ?on_icon ~icon () =
    let classes =
      classes
      |> cons_if on CSS.on
      |> List.cons CSS.root in
    a ~a:([a_class classes] <@> attrs
          |> map_cons_option a_href href
          |> cons_if_lazy ripple (fun () -> a_user_data "ripple" "true"))
      (on_icon ^:: icon :: [])

end
