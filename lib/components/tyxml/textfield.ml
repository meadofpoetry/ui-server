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
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module Notched_outline_markup = Notched_outline.Make (Xml) (Svg) (Html)
  module Floating_label_markup = Floating_label.Make (Xml) (Svg) (Html)
  module Line_ripple_markup = Line_ripple.Make (Xml) (Svg) (Html)

  let ( % ) f g x = f (g x)

  let ( @:: ) = cons

  let ( ^:: ) x l = Option.fold ~none:l ~some:(fun x -> cons x l) x

  module Helper_text = struct
    let helper_text
        ?(classes = return [])
        ?(a = [])
        ?(persistent = false)
        ?(validation = false)
        ?text
        ?(children = nil ())
        () =
      let classes =
        fmap
          (Utils.cons_if validation CSS.Helper_text.validation_msg
          % Utils.cons_if persistent CSS.Helper_text.persistent
          % List.cons CSS.Helper_text.root)
          classes
      in
      let children =
        match text with
        | None -> children
        | Some x -> cons (return (txt x)) children
      in
      div
        ~a:
          (a_class classes :: a
          |> Utils.cons_if (not persistent) @@ a_aria "hidden" (return ["true"]))
        children
  end

  module Character_counter = struct
    let character_counter
        ?(classes = return [])
        ?(a = [])
        ?(current_length = 0)
        ?(max_length = 0)
        () =
      let classes = fmap (fun x -> CSS.Character_counter.root :: x) classes in
      let text = return @@ Printf.sprintf "%d / %d" current_length max_length in
      div ~a:(a_class classes :: a) (singleton (return (txt text)))
  end

  module Textarea = struct
    let textarea_input
        ?(classes = return [])
        ?(a = [])
        ?id
        ?(value = return "")
        ?placeholder
        ?(required = false)
        ?min_length
        ?max_length
        ?rows
        ?cols
        ?(disabled = false)
        () =
      let classes = fmap (fun x -> CSS.input :: x) classes in
      textarea
        ~a:
          Utils.(
            a_class classes :: a
            |> map_cons_option a_id id
            |> cons_if_lazy disabled a_disabled
            |> cons_if_lazy required a_required
            |> map_cons_option a_placeholder placeholder
            |> map_cons_option (a_minlength % return) min_length
            |> map_cons_option (a_maxlength % return) max_length
            |> map_cons_option a_rows rows
            |> map_cons_option a_cols cols)
        (return (txt value))

    let textarea
        ?(classes = return [])
        ?(a = [])
        ?(disabled = false)
        ?(fullwidth = false)
        ?(focused = false)
        ?(show_character_counter = false)
        ?character_counter
        ?input_id
        ?label
        ?outline
        ?value
        ?placeholder
        ?required
        ?min_length
        ?max_length
        ?rows
        ?cols
        ?input
        () : 'a elt =
      let classes =
        fmap
          Utils.(
            cons_if (Option.is_none label) CSS.no_label
            % cons_if fullwidth CSS.fullwidth
            % cons_if disabled CSS.disabled
            % cons_if focused CSS.focused
            % List.cons CSS.textarea
            % List.cons CSS.root)
          classes
      in
      let cc =
        match character_counter, show_character_counter with
        | None, false -> None
        | (Some _ as x), _ -> x
        | None, true ->
            Some (return @@ Character_counter.character_counter ?max_length ())
      in
      let outline =
        match outline with
        | Some x -> return x
        | None ->
            return
            @@ Notched_outline_markup.notched_outline ?label_for:input_id ?label ()
      in
      let input =
        match input with
        | Some x -> return x
        | None ->
            return
            @@ textarea_input
                 ?id:input_id
                 ?value
                 ?placeholder
                 ?required
                 ?min_length
                 ?max_length
                 ?rows
                 ?cols
                 ()
      in
      div ~a:(a_class classes :: a) (cc ^:: input @:: outline @:: nil ())
  end

  let textfield_input
      ?(classes = return [])
      ?(a = [])
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
      () =
    let classes = fmap (fun x -> CSS.input :: x) classes in
    input
      ~a:
        Utils.(
          a_class classes :: a_input_type (return typ) :: a
          |> map_cons_option a_inputmode input_mode
          |> cons_if_lazy disabled a_disabled
          |> map_cons_option a_id id
          |> map_cons_option a_pattern pattern
          |> map_cons_option a_minlength min_length
          |> map_cons_option a_maxlength max_length
          |> map_cons_option (fun x -> a_step @@ return @@ Some x) step
          |> cons_if_lazy required a_required
          |> map_cons_option a_placeholder placeholder
          |> map_cons_option a_value value)
      ()

  let textfield_helper_line ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (fun x -> CSS.helper_line :: x) classes in
    div ~a:(a_class classes :: a) children

  let textfield
      ?(classes = return [])
      ?(a = [])
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
        textfield_input
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
      | None, true -> Some (return @@ Notched_outline_markup.notched_outline ?label ())
    in
    let line_ripple =
      match line_ripple, outline with
      | (Some _ as x), _ -> x
      | None, Some _ -> None
      | None, None -> Some (return @@ Line_ripple_markup.line_ripple ())
    in
    let floating_label =
      match label, outline with
      | None, _ -> None
      | Some _, Some _ -> None
      | Some x, None ->
          Some (return @@ Floating_label_markup.floating_label ?for_:input_id ~label:x ())
    in
    let with_leading_icon = Option.is_some leading_icon in
    let with_trailing_icon = Option.is_some trailing_icon in
    let classes =
      fmap
        Utils.(
          cons_if (Option.is_none label) CSS.no_label
          % cons_if textarea CSS.textarea
          % cons_if fullwidth CSS.fullwidth
          % cons_if disabled CSS.disabled
          % cons_if (Option.is_some outline) CSS.outlined
          % cons_if focused CSS.focused
          % cons_if with_leading_icon CSS.with_leading_icon
          % cons_if with_trailing_icon CSS.with_trailing_icon
          % List.cons CSS.root)
        classes
    in
    div
      ~a:(a_class classes :: a)
      (leading_icon
      ^:: return input
      @:: floating_label
      ^:: trailing_icon
      ^:: outline
      ^:: line_ripple
      ^:: nil ())
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
