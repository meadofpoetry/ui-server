open Utils

module CSS = struct
  (** Mandatory, for the parent element. *)
  let root = "mdc-switch"

  (** Mandatory, for the hidden input checkbox. *)
  let native_control = BEM.add_element root "native-control"

  (** Mandatory, for the track element. *)
  let track = BEM.add_element root "track"

  (** Mandatory, for the ripple effect. *)
  let thumb_underlay = BEM.add_element root "thumb-underlay"

  (** Mandatory, for the thumb element. *)
  let thumb = BEM.add_element root "thumb"

  (** Optional, styles the switch as checked ("on"). *)
  let checked = BEM.add_modifier root "checked"

  (** Optional, styles the switch as disabled. *)
  let disabled = BEM.add_modifier root "disabled"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create
      ?input_id
      ?(classes = [])
      ?(attrs = [])
      ?(checked = false)
      ?(disabled = false)
      () : 'a elt =
    let classes =
      classes
      |> cons_if checked CSS.checked
      |> cons_if disabled CSS.disabled
      |> List.cons CSS.root
    in
    div
      ~a:([a_class classes] @ attrs)
      [ div ~a:[a_class [CSS.track]] []
      ; div
          ~a:[a_class [CSS.thumb_underlay]]
          [ div
              ~a:[a_class [CSS.thumb]]
              [ input
                  ~a:
                    ([a_input_type `Checkbox; a_class [CSS.native_control]]
                    |> cons_if_lazy checked a_checked
                    |> cons_if_lazy disabled a_disabled
                    |> map_cons_option a_id input_id)
                  () ] ] ]
end
