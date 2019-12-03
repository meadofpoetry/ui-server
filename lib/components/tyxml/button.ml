type appearance =
  | Raised
  | Outlined
  | Unelevated

module CSS = struct
  (** Mandatory. Defaults to a text button that in flush with the surface. *)
  let root = "mdc-button"

  (** Optional. Indicatest the element containing the button's icon. *)
  let icon = BEM.add_element root "icon"

  (** Recommended. Indicates the element containing the button's text label. *)
  let label = BEM.add_element root "label"

  (** Optional. Indicates the element containing the loading indicator. *)
  let loader_container = BEM.add_element root "loader-container"

  (** Optional. Styles a contained button that is flush with the surface. *)
  let unelevated = BEM.add_modifier root "unelevated"

  (** Optional. Styles an outlined button that is flush with the surface. *)
  let outlined = BEM.add_modifier root "outlined"

  (** Optional. Styles a contained button that is elevated above the surface. *)
  let raised = BEM.add_modifier root "raised"

  (** Optional. Makes the button text and container slightly smaller. *)
  let dense = BEM.add_modifier root "dense"

  (** Optional. Styles a button to show loading indicator. *)
  let loading = BEM.add_modifier root "loading"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let ( ^:: ) x l = Option.fold ~none:l ~some:(fun x -> cons x l) x

  let button_loader_container ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (List.cons CSS.loader_container) classes in
    div ~a:(a_class classes :: a) children

  let button_ ?(classes = return []) ?appearance ?(dense = false) ?icon ?label () =
    let make_label x =
      span ~a:[a_class @@ return [CSS.label]] (singleton (return (txt x)))
    in
    let classes =
      fmap
        (Utils.map_cons_option
           (function
             | Raised -> CSS.raised
             | Outlined -> CSS.outlined
             | Unelevated -> CSS.unelevated)
           appearance
        % Utils.cons_if dense CSS.dense
        % List.cons CSS.root)
        classes
    in
    let label = Option.map (return % make_label) label in
    icon ^:: label ^:: nil (), classes

  let button_a ?classes ?(a = []) ?href ?appearance ?dense ?icon ?label () =
    let children, classes = button_ ?classes ?appearance ?dense ?icon ?label () in
    Html.a ~a:([a_class classes] @ a |> Utils.map_cons_option a_href href) children

  let button
      ?classes
      ?(a = [])
      ?button_type
      ?appearance
      ?on_click
      ?(disabled = false)
      ?dense
      ?icon
      ?label
      () =
    let children, classes = button_ ?classes ?appearance ?dense ?icon ?label () in
    button
      ~a:
        (a_class classes :: a
        |> Utils.map_cons_option a_onclick on_click
        |> Utils.map_cons_option a_button_type button_type
        |> Utils.cons_if disabled @@ a_disabled ())
      children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
