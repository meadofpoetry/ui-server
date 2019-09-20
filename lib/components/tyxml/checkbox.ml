module CSS = struct
  let root = "mdc-checkbox"

  let native_control = BEM.add_element root "native-control"

  let background = BEM.add_element root "background"

  let checkmark = BEM.add_element root "checkmark"

  let checkmark_path = BEM.add_element root "checkmark-path"

  let mixedmark = BEM.add_element root "mixedmark"

  let disabled = BEM.add_modifier root "disabled"

  let upgraded = BEM.add_modifier root "upgraded"

  (* Animation *)

  let anim_checked_indeterminate = BEM.add_modifier root "anim-checked-indeterminate"

  let anim_checked_unchecked = BEM.add_modifier root "anim-checked-unchecked"

  let anim_indeterminate_checked = BEM.add_modifier root "anim-indeterminate-checked"

  let anim_indeterminate_unchecked = BEM.add_modifier root "anim-indeterminate-unchecked"

  let anim_unchecked_checked = BEM.add_modifier root "anim-unchecked-checked"

  let anim_unchecked_indeterminate = BEM.add_modifier root "anim-unchecked-indeterminate"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let ( @:: ) = cons

  let checkbox
      ?(classes = return [])
      ?(a = [])
      ?input_id
      ?(disabled = false)
      ?(checked = false)
      () =
    let classes =
      fmap (Utils.cons_if disabled CSS.disabled % List.cons CSS.root) classes
    in
    let input =
      input
        ~a:
          ([a_input_type (return `Checkbox); a_class (return [CSS.native_control])]
          |> Utils.map_cons_option a_id input_id
          |> Utils.cons_if disabled @@ a_disabled ()
          |> Utils.cons_if checked @@ a_checked ())
        ()
    in
    let checkmark_path =
      Svg.path
        ~a:
          [ Svg.a_class (return [CSS.checkmark_path])
          ; Svg.a_fill (return `None)
          ; Svg.a_stroke (return (`Color ("white", None)))
          ; Svg.a_d (return "M1.73,12.91 8.1,19.28 22.79,4.59") ]
        (nil ())
    in
    let checkmark =
      svg
        ~a:
          [ Svg.a_class (return [CSS.checkmark])
          ; Svg.a_viewBox (return (0.0, 0.0, 24.0, 24.0)) ]
        (singleton (return checkmark_path))
    in
    let mixedmark = div ~a:[a_class (return [CSS.mixedmark])] (nil ()) in
    let background =
      div
        ~a:[a_class (return [CSS.background])]
        (return checkmark @:: return mixedmark @:: nil ())
    in
    div ~a:(a_class classes :: a) (return input @:: return background @:: nil ())
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
