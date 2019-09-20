module CSS = struct
  let root = "mdc-radio"

  let native_control = BEM.add_element root "native-control"

  let background = BEM.add_element root "background"

  let outer_circle = BEM.add_element root "outer-circle"

  let inner_circle = BEM.add_element root "inner-circle"

  let disabled = BEM.add_modifier root "disabled"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let radio_outer_circle ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (List.cons CSS.outer_circle) classes in
    div ~a:(a_class classes :: a) children

  let radio_inner_circle ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (List.cons CSS.inner_circle) classes in
    div ~a:(a_class classes :: a) children

  let radio_background
      ?(classes = return [])
      ?(a = [])
      ?outer_circle
      ?inner_circle
      ?children
      () =
    let classes = fmap (List.cons CSS.background) classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let outer_circle =
            match outer_circle with
            | Some x -> x
            | None -> radio_outer_circle ()
          in
          let inner_circle =
            match inner_circle with
            | Some x -> x
            | None -> radio_inner_circle ()
          in
          cons (return outer_circle) (singleton (return inner_circle))
    in
    div ~a:(a_class classes :: a) children

  let radio_native_control
      ?(classes = return [])
      ?(a = [])
      ?(checked = false)
      ?(disabled = false)
      ?input_id
      ?name
      () =
    let classes = fmap (List.cons CSS.native_control) classes in
    input
      ~a:
        (a_class classes :: a_input_type (return `Radio) :: a
        |> Utils.map_cons_option a_name name
        |> Utils.cons_if_lazy checked a_checked
        |> Utils.cons_if_lazy disabled a_disabled
        |> Utils.map_cons_option a_id input_id)
      ()

  let radio
      ?(classes = return [])
      ?(a = [])
      ?input_id
      ?checked
      ?(disabled = false)
      ?name
      ?outer_circle
      ?inner_circle
      ?background
      ?native_control
      ?children
      () : 'a elt =
    let classes =
      fmap (Utils.cons_if disabled CSS.disabled % List.cons CSS.root) classes
    in
    let children =
      match children with
      | Some x -> x
      | None ->
          let background =
            match background with
            | Some x -> x
            | None -> radio_background ?outer_circle ?inner_circle ()
          in
          let native_control =
            match native_control with
            | Some x -> x
            | None -> radio_native_control ?input_id ?checked ?name ~disabled ()
          in
          cons (return native_control) (singleton (return background))
    in
    div ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
