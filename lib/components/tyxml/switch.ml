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

  let create_track ?(classes = []) ?(attrs = []) ?(children = []) () =
    let classes = CSS.track :: classes in
    div ~a:([a_class classes] @ attrs) children

  let create_native_control
      ?(classes = [])
      ?(attrs = [])
      ?id
      ?(checked = false)
      ?(disabled = false)
      () =
    let classes = CSS.native_control :: classes in
    input
      ~a:
        ([a_input_type `Checkbox; a_class classes] @ attrs
        |> Utils.cons_if_lazy checked a_checked
        |> Utils.cons_if_lazy disabled a_disabled
        |> Utils.map_cons_option a_id id)
      ()

  let create_thumb
      ?(classes = [])
      ?(attrs = [])
      ?input_id
      ?checked
      ?disabled
      ?(native_control = create_native_control ?id:input_id ?checked ?disabled ())
      () =
    let classes = CSS.thumb :: classes in
    div ~a:([a_class classes] @ attrs) [native_control]

  let create_thumb_underlay
      ?(classes = [])
      ?(attrs = [])
      ?input_id
      ?checked
      ?disabled
      ?(thumb = create_thumb ?input_id ?checked ?disabled ())
      () =
    let classes = CSS.thumb_underlay :: classes in
    div ~a:([a_class classes] @ attrs) [thumb]

  let create
      ?input_id
      ?(classes = [])
      ?(attrs = [])
      ?(checked = false)
      ?(disabled = false)
      ?(track = create_track ())
      ?(thumb_underlay = create_thumb_underlay ?input_id ~checked ~disabled ())
      () : 'a elt =
    let classes =
      classes
      |> Utils.cons_if checked CSS.checked
      |> Utils.cons_if disabled CSS.disabled
      |> List.cons CSS.root
    in
    div ~a:([a_class classes] @ attrs) [track; thumb_underlay]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
