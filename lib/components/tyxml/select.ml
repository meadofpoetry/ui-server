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
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module CSS = CSS
  module Notched_outline = Notched_outline.Make (Xml) (Svg) (Html)
  module Floating_label = Floating_label.Make (Xml) (Svg) (Html)

  open Utils.Make (Xml)

  module Helper_text = struct
    let helper_text
        ?(classes = [])
        ?(a = [])
        ?(persistent = false)
        ?(validation = false)
        ?text
        ?(children = nil ())
        () : 'a elt =
      let classes =
        classes
        |> Utils.cons_if validation CSS.Helper_text.validation_msg
        |> Utils.cons_if persistent CSS.Helper_text.persistent
        |> List.cons CSS.Helper_text.root
      in
      let children =
        match text with
        | None -> children
        | Some x -> cons (return (txt x)) children
      in
      div
        ~a:
          (a_class (return classes) :: a
          |> Utils.cons_if (not persistent) @@ a_aria "hidden" (return ["true"]))
        children
  end

  module Native = struct
    let option
        ?(classes = [])
        ?(a = [])
        ?value
        ?label
        ?(disabled = false)
        ?(selected = false)
        ~text
        () : 'a elt =
      option
        ~a:
          (a_class (return classes) :: a
          |> Utils.map_cons_option a_value value
          |> Utils.cons_if_lazy disabled a_disabled
          |> Utils.cons_if_lazy selected a_selected
          |> Utils.map_cons_option a_label label)
        (return (txt text))

    let optgroup
        ?(classes = [])
        ?(a = [])
        ~label
        ?(disabled = false)
        ?(options = nil ())
        () =
      optgroup
        ~a:(a_class (return classes) :: a |> Utils.cons_if_lazy disabled a_disabled)
        ~label
        options

    let native_control
        ?(classes = [])
        ?(a = [])
        ?id
        ?(disabled = false)
        ?(autofocus = false)
        ?(required = false)
        ?size (* Number of visible options in a drop-down list *)
        ?form (* Form the select field belongs to *)
        ?name (* Name of the drop-down list *)
        ?(options = nil ())
        () =
      let classes = CSS.native_control :: classes in
      select
        ~a:
          (a_class (return classes) :: a
          |> Utils.map_cons_option a_id id
          |> Utils.map_cons_option a_size size
          |> Utils.map_cons_option a_name name
          |> Utils.map_cons_option a_form form
          |> Utils.cons_if_lazy required a_required
          |> Utils.cons_if_lazy autofocus a_autofocus
          |> Utils.cons_if_lazy disabled a_disabled)
        options

    let select
        ?(classes = [])
        ?(a = [])
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
        ?(native_control =
          native_control ?id:input_id ?required ?autofocus ?size ?form ?name ?options ())
        () : 'a elt =
      let outline =
        match outline, outlined with
        | Some x, _ -> Some (return x)
        | None, false -> None
        | None, true -> Some (return @@ Notched_outline.notched_outline ?label ())
      in
      let floating_label =
        match label, outline with
        | None, _ -> None
        | Some _, Some _ -> None
        | Some x, None ->
            Some (return @@ Floating_label.floating_label ?for_:input_id ~label:x ())
      in
      let classes =
        classes
        |> Utils.cons_if disabled CSS.disabled
        |> Utils.cons_if (Option.is_some outline) CSS.outlined
        |> List.cons CSS.root
      in
      let dropdown_icon = i ~a:[a_class (return [CSS.dropdown_icon])] (nil ()) in
      div
        ~a:(a_class (return classes) :: a)
        (icon
        ^:: return dropdown_icon
        @:: return native_control
        @:: floating_label
        ^:: line_ripple
        ^:: outline
        ^:: nil ())
  end

  module Enhanced = struct
    let hidden_input ?(classes = []) ?(a = []) ?(disabled = false) () : 'a elt =
      input
        ~a:
          (a_class (return classes) :: a_input_type (return `Hidden) :: a
          |> Utils.cons_if_lazy disabled a_disabled)
        ()

    let select
        ?(classes = [])
        ?(a = [])
        ?label
        ?line_ripple
        ?(disabled = false)
        ?(selected_text = return "")
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
      let dropdown_icon = i ~a:[a_class (return [CSS.dropdown_icon])] (nil ()) in
      let selected_text =
        div
          ~a:[a_class (return [CSS.selected_text])]
          (singleton (return (txt selected_text)))
      in
      div
        ~a:(a_class (return classes) :: a)
        (hidden_input
        ^:: icon
        ^:: return dropdown_icon
        @:: return selected_text
        @:: menu
        @:: label
        ^:: line_ripple
        ^:: outline
        ^:: nil ())
  end
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
