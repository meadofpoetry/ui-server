module CSS = struct
  (** Mandatory. *)
  let root = "mdc-select"

  (** Mandatory when using the enhanced select. This class should be
      placed on the mdc-menu element within the mdc-select element. *)
  let menu = BEM.add_element root "menu"

  (** Mandatory. Should be placed on an i element within the mdc-select
      element. Used for the dropdown arrow svg and animation. *)
  let dropdown_icon = BEM.add_element root "dropdown-icon"

  (** Optional. Should be placed on an i or svg element within the mdc-select
      element. Used for the leading icon. *)
  let icon = BEM.add_element root "icon"

  (** Optional. Styles the select as disabled. This class should be
      applied to the root element when the disabled attribute is applied
      to the <select> element. *)
  let disabled = BEM.add_modifier root "disabled"

  (** Optional. Styles the select as outlined select. *)
  let outlined = BEM.add_modifier root "outlined"

  (** Mandatory for the native select. The native <select> element. *)
  let native_control = BEM.add_element root "native-control"

  (** Mandatory for the enhanced select. This class should be placed on a div
      within the mdc-select element. *)
  let selected_text = BEM.add_element root "selected-text"

  (** Styles the select as a select with a leading icon. *)
  let with_leading_icon = BEM.add_modifier root "with-leading-icon"

  let is_changing = BEM.add_modifier root "is-changing"

  module Helper_text = struct
    (** Mandatory. *)
    let root = root ^ "-helper-text"

    (** Makes the helper text permanently visible. *)
    let persistent = BEM.add_modifier root "persistent"

    (** Indicates the helper text is a validation message. *)
    let validation_msg = BEM.add_modifier root "validation-msg"
  end
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  module Native = struct

    let create_option ?(classes = []) ?attrs ?(disabled = false)
          ?(selected = false) ~text () : 'a elt =
      option ~a:([a_class classes]
                 |> cons_if_lazy disabled a_disabled
                 |> cons_if_lazy selected a_selected
                 <@> attrs)
        (txt text)

    let create_optgroup ?(classes = []) ?attrs ~label ~items () : 'a elt =
      optgroup ~a:([a_class classes] <@> attrs) ~label items

    let create_select ?(classes = []) ?attrs
          ?(disabled = false) ~items () : 'a elt =
      let classes = CSS.native_control ::classes in
      select ~a:([a_class classes]
                 |> cons_if_lazy disabled a_disabled
                 <@> attrs) items

    let create ?(classes=[]) ?attrs ?label ?line_ripple ?(disabled = false)
          ?icon ~select () : 'a elt =
      let classes =
        classes
        |> cons_if disabled CSS.disabled
        |> List.cons CSS.root in
      let dropdown_icon = i ~a:[a_class [CSS.dropdown_icon]] [] in
      div ~a:([a_class classes] <@> attrs)
        (icon ^:: (dropdown_icon :: select :: (label ^:: line_ripple ^:: [])))
  end

  module Enhanced = struct

    let create_input ?(classes = []) ?attrs ?(disabled = false) () : 'a elt =
      input ~a:([ a_class classes
                ; a_input_type `Hidden ]
                |> cons_if_lazy disabled a_disabled
                <@> attrs) ()

    let create ?(classes = []) ?attrs ?label ?line_ripple
          ?(disabled = false) ?(selected_text = "") ?outline
          ?icon ~menu () : 'a elt =
      let with_leading_icon = match icon with
        | None -> false
        | Some _ -> true in
      let outlined = match outline with
        | None -> false
        | Some _ -> true in
      let classes =
        classes
        |> cons_if with_leading_icon CSS.with_leading_icon
        |> cons_if outlined CSS.outlined
        |> cons_if disabled CSS.disabled
        |> List.cons CSS.root in
      let dropdown_icon = i ~a:[a_class [CSS.dropdown_icon]] [] in
      let selected_text =
        div ~a:[a_class [CSS.selected_text]]
          [txt selected_text] in
      div ~a:([a_class classes] <@> attrs)
        (icon ^:: (dropdown_icon :: selected_text :: menu
                   :: (label ^:: line_ripple ^:: outline ^:: [])))

  end
end
