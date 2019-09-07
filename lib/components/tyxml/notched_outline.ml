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

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Floating_label_markup = Floating_label.Make (Xml) (Svg) (Html)

  let create_leading ?(classes = []) ?(attrs = []) () =
    let classes = CSS.leading :: classes in
    div ~a:([a_class classes] @ attrs) []

  let create_trailing ?(classes = []) ?(attrs = []) () =
    let classes = CSS.trailing :: classes in
    div ~a:([a_class classes] @ attrs) []

  let create_notch ?(classes = []) ?(attrs = []) ?label_for ~label () =
    let label =
      match label with
      | `Text s -> Floating_label_markup.create ?for_:label_for ~label:s ()
      | `Element e -> e
    in
    let classes = CSS.notch :: classes in
    div ~a:([a_class classes] @ attrs) [label]

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(leading = create_leading ())
      ?(trailing = create_trailing ())
      ?notch
      ?label_for
      ?label
      () : 'a elt =
    let classes = CSS.root :: classes in
    let notch =
      match notch with
      | Some _ as x -> x
      | None -> (
        match label with
        | None -> None
        | Some label -> Some (create_notch ?label_for ~label ()))
    in
    let content = leading :: Utils.(notch ^:: [trailing]) in
    div ~a:([a_class classes] @ attrs) content
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
