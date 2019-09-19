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
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module CSS = CSS

  let switch_track ?(classes = []) ?(a = []) ?(children = nil ()) () =
    let classes = CSS.track :: classes in
    div ~a:(a_class (return classes) :: a) children

  let switch_native_control
      ?(classes = [])
      ?(a = [])
      ?id
      ?(checked = false)
      ?(disabled = false)
      () =
    let classes = CSS.native_control :: classes in
    input
      ~a:
        (a_input_type (return `Checkbox) :: a_class (return classes) :: a
        |> Utils.cons_if_lazy checked a_checked
        |> Utils.cons_if_lazy disabled a_disabled
        |> Utils.map_cons_option a_id id)
      ()

  let switch_thumb
      ?(classes = [])
      ?(a = [])
      ?input_id
      ?checked
      ?disabled
      ?(native_control = switch_native_control ?id:input_id ?checked ?disabled ())
      () =
    let classes = CSS.thumb :: classes in
    div ~a:(a_class (return classes) :: a) (singleton (return native_control))

  let switch_thumb_underlay
      ?(classes = [])
      ?(a = [])
      ?input_id
      ?checked
      ?disabled
      ?(thumb = switch_thumb ?input_id ?checked ?disabled ())
      () =
    let classes = CSS.thumb_underlay :: classes in
    div ~a:(a_class (return classes) :: a) (singleton (return thumb))

  let switch
      ?input_id
      ?(classes = [])
      ?(a = [])
      ?(checked = false)
      ?(disabled = false)
      ?(track = switch_track ())
      ?(thumb_underlay = switch_thumb_underlay ?input_id ~checked ~disabled ())
      () : 'a elt =
    let classes =
      classes
      |> Utils.cons_if checked CSS.checked
      |> Utils.cons_if disabled CSS.disabled
      |> List.cons CSS.root
    in
    div
      ~a:(a_class (return classes) :: a)
      (cons (return track) (cons (return thumb_underlay) (nil ())))
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
