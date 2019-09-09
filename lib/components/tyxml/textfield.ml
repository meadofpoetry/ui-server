module CSS = struct
  let root = "mdc-text-field"

  let outlined = BEM.add_modifier root "outlined"

  let fullwidth = BEM.add_modifier root "fullwidth"

  let textarea = BEM.add_modifier root "textarea"

  let disabled = BEM.add_modifier root "disabled"

  let invalid = BEM.add_modifier root "invalid"

  let with_leading_icon = BEM.add_modifier root "with-leading-icon"

  let with_trailing_icon = BEM.add_modifier root "with-trailing-icon"

  let focused = BEM.add_modifier root "focused"

  let no_label = BEM.add_modifier root "no-label"

  let helper_line = root ^ "-helper-line"

  let icon = BEM.add_element root "icon"

  let input = BEM.add_element root "input"

  module Helper_text = struct
    let root = root ^ "-helper-text"

    let persistent = BEM.add_modifier root "persistent"

    let validation_msg = BEM.add_modifier root "validation-msg"
  end

  module Character_counter = struct
    let root = root ^ "-character-counter"
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
  module Line_ripple_markup = Line_ripple.Make (Xml) (Svg) (Html)

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

  module Character_counter = struct
    let create ?(classes = []) ?(attrs = []) ?(current_length = 0) ?(max_length = 0) () =
      let classes = CSS.Character_counter.root :: classes in
      let text = Printf.sprintf "%d / %d" current_length max_length in
      div ~a:([a_class classes] @ attrs) [txt text]
  end

  module Textarea = struct
    let create_textarea
        ?(classes = [])
        ?(attrs = [])
        ?id
        ?(value = "")
        ?placeholder
        ?(required = false)
        ?min_length
        ?max_length
        ?rows
        ?cols
        ?(disabled = false)
        () : 'a elt =
      let classes = CSS.input :: classes in
      textarea
        ~a:
          Utils.(
            [a_class classes] @ attrs
            |> map_cons_option a_id id
            |> cons_if_lazy disabled a_disabled
            |> cons_if_lazy required a_required
            |> map_cons_option a_placeholder placeholder
            |> map_cons_option a_minlength min_length
            |> map_cons_option a_maxlength max_length
            |> map_cons_option a_rows rows
            |> map_cons_option a_cols cols)
        (txt value)

    let create
        ?(classes = [])
        ?(attrs = [])
        ?(disabled = false)
        ?(fullwidth = false)
        ?(focused = false)
        ?(show_character_counter = false)
        ?character_counter
        ?input_id
        ?label
        ?(outline = Notched_outline_markup.create ?label_for:input_id ?label ())
        ?value
        ?placeholder
        ?required
        ?min_length
        ?max_length
        ?rows
        ?cols
        ?(input =
          create_textarea
            ?id:input_id
            ?value
            ?placeholder
            ?required
            ?min_length
            ?max_length
            ?rows
            ?cols
            ())
        () : 'a elt =
      let cc =
        match character_counter, show_character_counter with
        | None, false -> None
        | (Some _ as x), _ -> x
        | None, true -> Some (Character_counter.create ?max_length ())
      in
      let classes =
        Utils.(
          classes
          |> cons_if (Option.is_none label) CSS.no_label
          |> cons_if fullwidth CSS.fullwidth
          |> cons_if disabled CSS.disabled
          |> cons_if focused CSS.focused
          |> List.cons CSS.textarea
          |> List.cons CSS.root)
      in
      div ~a:([a_class classes] @ attrs) Utils.(cc ^:: [input; outline])
  end

  let create_input
      ?(classes = [])
      ?(attrs = [])
      ?id
      ?pattern
      ?min_length
      ?max_length
      ?step
      ?value
      ?placeholder
      ?(required = false)
      ?(disabled = false)
      ?(typ = `Text)
      ?input_mode
      () : 'a elt =
    let classes = CSS.input :: classes in
    input
      ~a:
        Utils.(
          [a_class classes; a_input_type typ] @ attrs
          |> map_cons_option a_inputmode input_mode
          |> cons_if_lazy disabled a_disabled
          |> map_cons_option a_id id
          |> map_cons_option a_pattern pattern
          |> map_cons_option a_minlength min_length
          |> map_cons_option a_maxlength max_length
          |> map_cons_option (fun x -> a_step @@ Some x) step
          |> cons_if_lazy required a_required
          |> map_cons_option a_placeholder placeholder
          |> map_cons_option a_value value)
      ()

  let create_helper_line ?(classes = []) ?(attrs = []) ?(children = []) () : 'a elt =
    let classes = CSS.helper_line :: classes in
    div ~a:([a_class classes] @ attrs) children

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(disabled = false)
      ?leading_icon
      ?trailing_icon
      ?(fullwidth = false)
      ?(textarea = false)
      ?(focused = false)
      ?label
      ?outline
      ?(outlined = false)
      ?line_ripple
      ?input_id
      ?pattern
      ?min_length
      ?max_length
      ?step
      ?value
      ?placeholder
      ?required
      ?typ
      ?input_mode
      ?(input =
        create_input
          ?id:input_id
          ?pattern
          ?min_length
          ?max_length
          ?step
          ?value
          ?placeholder
          ?required
          ?typ
          ?input_mode
          ())
      () : 'a elt =
    let outline =
      match outline, outlined with
      | (Some _ as x), _ -> x
      | None, false -> None
      | None, true -> Some (Notched_outline_markup.create ?label ())
    in
    let line_ripple =
      match line_ripple, outline with
      | (Some _ as x), _ -> x
      | None, Some _ -> None
      | None, None -> Some (Line_ripple_markup.create ())
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
    let with_leading_icon = Option.is_some leading_icon in
    let with_trailing_icon = Option.is_some trailing_icon in
    let classes =
      Utils.(
        classes
        |> cons_if (Option.is_none label) CSS.no_label
        |> cons_if textarea CSS.textarea
        |> cons_if fullwidth CSS.fullwidth
        |> cons_if disabled CSS.disabled
        |> cons_if (Option.is_some outline) CSS.outlined
        |> cons_if focused CSS.focused
        |> cons_if with_leading_icon CSS.with_leading_icon
        |> cons_if with_trailing_icon CSS.with_trailing_icon
        |> List.cons CSS.root)
    in
    div
      ~a:([a_class classes] @ attrs)
      Utils.(
        leading_icon
        ^:: (input
            :: (floating_label ^:: trailing_icon ^:: outline ^:: line_ripple ^:: [])))
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
