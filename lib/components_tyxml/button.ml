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

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create_loader_container ?(classes = []) ?attrs loader () : 'a elt =
    let classes = CSS.loader_container :: classes in
    div ~a:([a_class classes] <@> attrs) [loader]

  let create ?(classes = []) ?attrs ?button_type ?appearance
        ?(disabled = false) ?(dense = false) ?icon ?label () : 'a elt =
    let make_label (x : string) : _ elt =
      span ~a:[a_class [CSS.label]] [txt x] in
    let (classes : string list) =
      classes
      |> map_cons_option (function
             | Raised -> CSS.raised
             | Outlined -> CSS.outlined
             | Unelevated -> CSS.unelevated) appearance
      |> cons_if dense CSS.dense
      |> List.cons CSS.root in
    button ~a:([a_class classes]
               |> map_cons_option a_button_type button_type
               |> cons_if disabled @@ a_disabled ()
               <@> attrs)
      (cons_option icon @@ map_cons_option make_label label [])
end
