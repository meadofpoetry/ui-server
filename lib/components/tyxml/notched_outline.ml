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
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module CSS = CSS
  module Floating_label = Floating_label.Make (Xml) (Svg) (Html)

  let ( ^:: ) x l =
    match x with
    | None -> l
    | Some x -> cons x l

  let notched_outline_leading ?(classes = []) ?(a = []) ?(children = nil ()) () =
    let classes = CSS.leading :: classes in
    div ~a:(a_class (return classes) :: a) children

  let notched_outline_trailing ?(classes = []) ?(a = []) ?(children = nil ()) () =
    let classes = CSS.trailing :: classes in
    div ~a:(a_class (return classes) :: a) children

  let notched_outline_notch
      ?(classes = [])
      ?(a = [])
      ?label_for
      ?label
      ?(children = nil ())
      () =
    let children =
      match label with
      | None -> children
      | Some x ->
          let label = Floating_label.floating_label ?for_:label_for ~label:x () in
          cons (return label) children
    in
    let classes = CSS.notch :: classes in
    div ~a:(a_class (return classes) :: a) children

  let notched_outline
      ?(classes = [])
      ?(a = [])
      ?leading
      ?trailing
      ?notch
      ?label_for
      ?label
      ?children
      () : 'a elt =
    let classes = CSS.root :: classes in
    let notch =
      match notch with
      | Some _ as x -> x
      | None -> (
        match label with
        | None -> None
        | Some label -> Some (return @@ notched_outline_notch ?label_for ~label ()))
    in
    let children =
      match children with
      | Some x -> x
      | None ->
          let leading =
            match leading with
            | Some x -> x
            | None -> return @@ notched_outline_leading ()
          in
          let trailing =
            match trailing with
            | Some x -> x
            | None -> return @@ notched_outline_trailing ()
          in
          cons leading (notch ^:: singleton trailing)
    in
    div ~a:(a_class (return classes) :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
