open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-list"
  let dense_class = CSS.add_modifier base_class "dense"
  let two_line_class = CSS.add_modifier base_class "two-line"
  let avatar_class = CSS.add_modifier base_class "avatar-list"
  let non_interactive_class = CSS.add_modifier base_class "non-interactive"

  module Item = struct

    let _class = "mdc-list-item"
    let text_class = CSS.add_element _class "text"
    let primary_text_class = CSS.add_element _class "primary-text"
    let secondary_text_class = CSS.add_element _class "secondary-text"
    let divider_class = "mdc-list-divider"

    let graphic_class = CSS.add_element _class "graphic"
    let meta_class = CSS.add_element _class "meta"

    let selected_class = CSS.add_modifier _class "selected"
    let activated_class = CSS.add_modifier _class "activated"

    let create_divider ?(classes = []) ?attrs
          ?(tag = div) ?(inset = false) () : 'a elt =
      let (classes : string list) =
        classes
        |> cons_if inset @@ CSS.add_modifier divider_class "inset"
        |> List.cons divider_class in
      tag ~a:([ a_class classes
              ; a_role ["separator"]] <@> attrs) []

    let create_primary_text ?(classes = []) ?attrs text () : 'a elt =
      span ~a:([a_class (primary_text_class :: classes)] <@> attrs)
        [txt text]

    let create_secondary_text ?(classes = []) ?attrs text () : 'a elt =
      span ~a:([a_class (secondary_text_class :: classes)] <@> attrs)
        [txt text]

    let create_text ?(classes = []) ?attrs ~primary ~secondary () : 'a elt =
      span ~a:([ a_class (text_class :: classes)] <@> attrs)
        [primary; secondary]

    let create_text_simple text () : 'a elt =
      txt text

    let create ?(classes = []) ?attrs
          ?(tag = div) ?graphic ?meta text () : 'a elt =
      tag ~a:([a_class (_class :: classes)] <@> attrs)
        (graphic ^:: (text :: (meta ^:: [])))

  end

  module List_group = struct

    let _class = "mdc-list-group"
    let subheader_class = CSS.add_element _class "subheader"

    let create_subheader ?(classes = []) ?attrs ?(tag = h3) ~text () : 'a elt =
      tag ~a:([a_class (subheader_class :: classes)] <@> attrs) [txt text]

    let create ?(classes = []) ?attrs ~content () =
      div ~a:([ a_class (_class :: classes)] <@> attrs) content

  end

  let create ?(classes = []) ?(tag = div) ?attrs
        ?(avatar = false) ?(dense = false) ?(two_line = false)
        ~items () : 'a elt =
    let classes =
      classes
      |> cons_if dense dense_class
      |> cons_if two_line two_line_class
      |> cons_if avatar avatar_class
      |> List.cons base_class in
    tag ~a:([a_class classes] <@> attrs) items

end
