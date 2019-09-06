module CSS = struct
  let root = "mdc-linear-progress"

  let buffering_dots = BEM.add_element root "buffering-dots"

  let buffer = BEM.add_element root "buffer"

  let bar = BEM.add_element root "bar"

  let primary_bar = BEM.add_element root "primary-bar"

  let secondary_bar = BEM.add_element root "secondary-bar"

  let bar_inner = BEM.add_element root "bar-inner"

  (** Puts the linear progress indicator in an indeterminate state. *)
  let indeterminate = BEM.add_modifier root "indeterminate"

  (** Reverses the direction of the linear progress indicator. *)
  let reversed = BEM.add_modifier root "reversed"

  (** Hides the linear progress indicator. *)
  let closed = BEM.add_modifier root "closed"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  open Utils

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(indeterminate = false)
      ?(reversed = false)
      ?(closed = false)
      () : 'a elt =
    let classes =
      classes
      |> cons_if closed CSS.closed
      |> cons_if indeterminate CSS.indeterminate
      |> cons_if reversed CSS.reversed
      |> List.cons CSS.root
    in
    div
      ~a:([a_role ["progressbar"]; a_class classes] @ attrs)
      [ div ~a:[a_class [CSS.buffering_dots]] []
      ; div ~a:[a_class [CSS.buffer]] []
      ; div ~a:[a_class [CSS.bar; CSS.primary_bar]] [span ~a:[a_class [CSS.bar_inner]] []]
      ; div
          ~a:[a_class [CSS.bar; CSS.secondary_bar]]
          [span ~a:[a_class [CSS.bar_inner]] []] ]
end
