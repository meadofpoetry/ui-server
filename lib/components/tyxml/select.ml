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

  let focused = BEM.add_modifier root "focused"

  let required = BEM.add_modifier root "required"

  let invalid = BEM.add_modifier root "invalid"

  let activated = BEM.add_modifier root "activated"

  module Helper_text = struct
    (** Mandatory. *)
    let root = root ^ "-helper-text"

    (** Makes the helper text permanently visible. *)
    let persistent = BEM.add_modifier root "persistent"

    (** Indicates the helper text is a validation message. *)
    let validation_msg = BEM.add_modifier root "validation-msg"
  end
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Notched_outline_markup = Notched_outline.Make (Xml) (Svg) (Html)
  module Floating_label_markup = Floating_label.Make (Xml) (Svg) (Html)

  module Helper_text = struct
    let create
        ?(classes = [])
        ?(attrs = [])
        ?(persistent = false)
        ?(validation = false)
        ?text
        ?(children = [])
        () : 'a elt =
      let classes =
        classes
        |> Utils.cons_if validation CSS.Helper_text.validation_msg
        |> Utils.cons_if persistent CSS.Helper_text.persistent
        |> List.cons CSS.Helper_text.root
      in
      div
        ~a:
          ([a_class classes] @ attrs
          |> Utils.cons_if (not persistent) @@ a_aria "hidden" ["true"])
        (Utils.map_cons_option txt text children)
  end

  module Native = struct
    let create_option
        ?(classes = [])
        ?(attrs = [])
        ?value
        ?label
        ?(disabled = false)
        ?(selected = false)
        ~text
        () : 'a elt =
      option
        ~a:
          ([a_class classes] @ attrs
          |> Utils.map_cons_option a_value value
          |> Utils.cons_if_lazy disabled a_disabled
          |> Utils.cons_if_lazy selected a_selected
          |> Utils.map_cons_option a_label label)
        (txt text)

    let create_optgroup
        ?(classes = [])
        ?(attrs = [])
        ~label
        ?(disabled = false)
        ?(options = [])
        () =
      optgroup
        ~a:([a_class classes] @ attrs |> Utils.cons_if_lazy disabled a_disabled)
        ~label
        options

    let create_select
        ?(classes = [])
        ?(attrs = [])
        ?id
        ?(disabled = false)
        ?(autofocus = false)
        ?(required = false)
        ?size (* Number of visible options in a drop-down list *)
        ?form (* Form the select field belongs to *)
        ?name (* Name of the drop-down list *)
        ?(options = [])
        () =
      let classes = CSS.native_control :: classes in
      select
        ~a:
          ([a_class classes] @ attrs
          |> Utils.map_cons_option a_id id
          |> Utils.map_cons_option a_size size
          |> Utils.map_cons_option a_name name
          |> Utils.map_cons_option a_form form
          |> Utils.cons_if_lazy required a_required
          |> Utils.cons_if_lazy autofocus a_autofocus
          |> Utils.cons_if_lazy disabled a_disabled)
        options

    let create
        ?(classes = [])
        ?(attrs = [])
        ?label
        ?input_id
        ?line_ripple
        ?(disabled = false)
        ?(outlined = false)
        ?outline
        ?icon
        ?required
        ?autofocus
        ?size
        ?form
        ?name
        ?options
        ?(select =
          create_select ?id:input_id ?required ?autofocus ?size ?form ?name ?options ())
        () : 'a elt =
      let outline =
        match outline, outlined with
        | (Some _ as x), _ -> x
        | None, false -> None
        | None, true -> Some (Notched_outline_markup.create ?label ())
      in
      let floating_label =
        match label, outline with
        | None, _ -> None
        | Some _, Some _ -> None
        | Some x, None -> (
          match x with
          | `Text s -> Some (Floating_label_markup.create ?for_:input_id ~label:s ())
          | `Element e -> Some e)
      in
      let classes =
        classes
        |> Utils.cons_if disabled CSS.disabled
        |> Utils.cons_if (Option.is_some outline) CSS.outlined
        |> List.cons CSS.root
      in
      let dropdown_icon = i ~a:[a_class [CSS.dropdown_icon]] [] in
      div
        ~a:([a_class classes] @ attrs)
        Utils.(
          icon
          ^:: (dropdown_icon
              :: select
              :: (floating_label ^:: line_ripple ^:: outline ^:: [])))
  end

  module Enhanced = struct
    let create_hidden_input ?(classes = []) ?(attrs = []) ?(disabled = false) () : 'a elt
        =
      input
        ~a:
          ([a_class classes; a_input_type `Hidden] @ attrs
          |> Utils.cons_if_lazy disabled a_disabled)
        ()

    let create
        ?(classes = [])
        ?(attrs = [])
        ?label
        ?line_ripple
        ?(disabled = false)
        ?(selected_text = "")
        ?outline
        ?icon
        ?hidden_input
        ~menu
        () : 'a elt =
      let with_leading_icon =
        match icon with
        | None -> false
        | Some _ -> true
      in
      let outlined =
        match outline with
        | None -> false
        | Some _ -> true
      in
      let classes =
        classes
        |> Utils.cons_if with_leading_icon CSS.with_leading_icon
        |> Utils.cons_if outlined CSS.outlined
        |> Utils.cons_if disabled CSS.disabled
        |> List.cons CSS.root
      in
      let dropdown_icon = i ~a:[a_class [CSS.dropdown_icon]] [] in
      let selected_text = div ~a:[a_class [CSS.selected_text]] [txt selected_text] in
      div
        ~a:([a_class classes] @ attrs)
        Utils.(
          hidden_input
          ^:: icon
          ^:: (dropdown_icon
              :: selected_text
              :: menu
              :: (label ^:: line_ripple ^:: outline ^:: [])))
  end
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
