open Utils
open Containers

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-text-field"
  let outlined_class = CSS.add_modifier base_class "outlined"
  let full_width_class = CSS.add_modifier base_class "fullwidth"
  let textarea_class = CSS.add_modifier base_class "textarea"
  let disabled_class = CSS.add_modifier base_class "disabled"
  let invalid_class = CSS.add_modifier base_class "invalid"
  let dense_class = CSS.add_modifier base_class "dense"
  let with_leading_icon_class = CSS.add_modifier base_class "with-leading-icon"
  let with_trailing_icon_class = CSS.add_modifier base_class "with-trailing-icon"
  let focused_class = CSS.add_modifier base_class "focused"
  let input_class = CSS.add_element base_class "input"
  let container_class = "mdc-text-field-container"

  module Helper_text = struct

    let _class = "mdc-text-field-helper-text"
    let persistent_class = CSS.add_modifier _class "persistent"
    let validation_msg_class = CSS.add_modifier _class "validation-msg"

    let create ?(classes = []) ?attrs
          ?(persistent = false) ?(validation = false) ?text () : 'a elt =
      let classes =
        classes
        |> cons_if validation validation_msg_class
        |> cons_if persistent persistent_class
        |> List.cons _class in
      p ~a:([a_class classes]
            |> cons_if (not persistent) @@ a_aria "hidden" ["true"]
            <@> attrs) [txt @@ Option.get_or ~default:"" text]
  end

  module Icon = struct

    let _class = CSS.add_element base_class "icon"

  end

  let create_input ?(classes = []) ?attrs ?input_id
        ?value ?placeholder ?(required = false)
        ?(input_type = `Text) () : 'a elt =
    input ~a:([ a_class (input_class :: classes)
              ; a_input_type input_type ]
              |> map_cons_option a_id input_id
              |> cons_if required @@ a_required ()
              |> map_cons_option a_placeholder placeholder
              |> map_cons_option a_value value
              <@> attrs) ()

  let create_textarea ?(classes = []) ?attrs ?input_id
        ?value ?placeholder ?(required = false)
        ?rows ?cols () : 'a elt =
    textarea ~a:([a_class (input_class :: classes)]
                 |> map_cons_option a_id input_id
                 |> cons_if required @@ a_required ()
                 |> map_cons_option a_placeholder placeholder
                 |> map_cons_option a_rows rows
                 |> map_cons_option a_cols cols
                 <@> attrs)
      (txt @@ Option.get_or ~default:"" value)

  let create ?(classes = []) ?attrs ?(disabled = false)
        ?leading_icon ?trailing_icon
        ?(full_width = false) ?(dense = false)
        ?(textarea = false)  ?(focused = false)
        ?line_ripple ?label ?outline ~input () : 'a elt =
    let classes =
      classes
      |> cons_if textarea textarea_class
      |> cons_if full_width full_width_class
      |> cons_if dense dense_class
      |> cons_if disabled disabled_class
      |> cons_if (Option.is_some outline) outlined_class
      |> cons_if focused focused_class
      |> cons_if (Option.is_some leading_icon) with_leading_icon_class
      |> cons_if (Option.is_some trailing_icon) with_trailing_icon_class
      |> List.cons base_class in
    let outline, outline_idle = match outline with
      | Some (x, y) -> Some x, Some y
      | None -> None, None in
    div ~a:([a_class classes] <@> attrs)
      ((label ^:: leading_icon ^:: trailing_icon ^:: [input])
       @ (outline ^:: outline_idle ^:: line_ripple ^:: []))

end
