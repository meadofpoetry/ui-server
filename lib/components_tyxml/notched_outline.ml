module CSS = struct
  (** Mandatory. Container for the outline's sides and notch. *)
  let root = "mdc-notched-outline"

  (** Mandatory. Element representing the leading side of the notched outline
      (before the notch). *)
  let leading = BEM.add_element root "leading"

  (** Mandatory. Element representing the notch. *)
  let notch = BEM.add_element root "notch"

  (** Mandatory. Element representing the trailing side of the notched outline
      (after the notch). *)
  let trailing = BEM.add_element root "trailing"

  (** Modifier class to open the notched outline. *)
  let notched = BEM.add_modifier root "notched"

  (** Modifier class to use when the floating label is empty or not used. *)
  let no_label = BEM.add_modifier root "no-label"

  let upgraded = BEM.add_modifier root "upgraded"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create ?(classes = []) ?(attrs = []) ?label () : 'a elt =
    let classes = CSS.root :: classes in
    let leading = div ~a:[a_class [CSS.leading]] [] in
    let trailing = div ~a:[a_class [CSS.trailing]] [] in
    let notch = match label with
      | None -> None
      | Some x -> Some (div ~a:[a_class [CSS.notch]] [x]) in
    let content = leading :: (notch ^:: (trailing :: [])) in
    div ~a:([a_class classes] @ attrs) content

end
