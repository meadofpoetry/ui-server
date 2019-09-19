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

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module CSS = CSS

  let ( ^:: ) x l =
    match x with
    | None -> l
    | Some x -> Xml.W.cons x l

  let icon_button
      ?(classes = [])
      ?(a = [])
      ?(ripple = true)
      ?(on = false)
      ?(disabled = false)
      ?on_icon
      ~icon
      () : 'a elt =
    let classes =
      Xml.W.return (classes |> Utils.cons_if on CSS.on |> List.cons CSS.root)
    in
    button
      ~a:
        (a_class classes :: a
        |> Utils.cons_if_lazy ripple (fun () ->
               a_user_data "ripple" (Xml.W.return "true"))
        |> Utils.cons_if_lazy disabled a_disabled)
      (on_icon ^:: Xml.W.cons icon (Xml.W.nil ()))

  let icon_button_a
      ?(classes = [])
      ?(a = [])
      ?href
      ?(ripple = true)
      ?(on = false)
      ?on_icon
      ~icon
      () =
    let classes =
      Xml.W.return (classes |> Utils.cons_if on CSS.on |> List.cons CSS.root)
    in
    Html.a
      ~a:
        (a_class classes :: a
        |> Utils.map_cons_option a_href href
        |> Utils.cons_if_lazy ripple (fun () ->
               a_user_data "ripple" (Xml.W.return "true")))
      (on_icon ^:: Xml.W.cons icon (Xml.W.nil ()))
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
