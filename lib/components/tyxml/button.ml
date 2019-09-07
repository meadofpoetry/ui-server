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
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_loader_container ?(classes = []) ?(attrs = []) loader () : 'a elt =
    let classes = CSS.loader_container :: classes in
    div ~a:([a_class classes] @ attrs) [loader]

  let create_ ?(classes = []) ?appearance ?(dense = false) ?icon ?label () =
    let make_label (x : string) : _ elt = span ~a:[a_class [CSS.label]] [txt x] in
    let (classes : string list) =
      classes
      |> Utils.map_cons_option
           (function
             | Raised -> CSS.raised
             | Outlined -> CSS.outlined
             | Unelevated -> CSS.unelevated)
           appearance
      |> Utils.cons_if dense CSS.dense
      |> List.cons CSS.root
    in
    Utils.cons_option icon @@ Utils.map_cons_option make_label label [], classes

  let create_anchor ?classes ?(attrs = []) ?href ?appearance ?dense ?icon ?label () =
    let children, classes = create_ ?classes ?appearance ?dense ?icon ?label () in
    a ~a:([a_class classes] @ attrs |> Utils.map_cons_option a_href href) children

  let create
      ?classes
      ?(attrs = [])
      ?button_type
      ?appearance
      ?(disabled = false)
      ?dense
      ?icon
      ?label
      () =
    let children, classes = create_ ?classes ?appearance ?dense ?icon ?label () in
    button
      ~a:
        ([a_class classes] @ attrs
        |> Utils.map_cons_option a_button_type button_type
        |> Utils.cons_if disabled @@ a_disabled ())
      children
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
