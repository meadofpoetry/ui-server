open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class   = "mdc-list"

  module Item = struct

    let _class               = "mdc-list-item"
    let text_class           = CSS.add_element _class "text"
    let secondary_text_class = CSS.add_element _class "secondary-text"
    let divider_class        = "mdc-list-divider"

    let graphic_class        = CSS.add_element _class "graphic"
    let meta_class           = CSS.add_element _class "meta"

    let create_divider ?(classes=[]) ?attrs ?(tag=li) ?(inset=false) () =
      tag ~a:([ a_class (classes
                         |> cons_if inset @@ CSS.add_modifier divider_class "inset"
                         |> List.cons divider_class)
              ; a_role ["separator"] ] <@> attrs) []

    let create_secondary_text ?(classes=[]) ?attrs text () =
      span ~a:([ a_class (secondary_text_class :: classes)] <@> attrs)
        [ pcdata text]

    let create_text ?(classes=[]) ?attrs ~secondary text () =
      span ~a:([ a_class (text_class :: classes)] <@> attrs)
        [ pcdata text; secondary ]

    let create_text_simple text () =
      pcdata text

    let create ?(classes=[]) ?attrs ?(tag=li) ?graphic ?meta text () =
      tag ~a:([ a_class (_class :: classes)] <@> attrs)
        (text :: (meta ^:: graphic ^:: []))

  end

  module List_group = struct

    let _class          = "mdc-list-group"
    let subheader_class = CSS.add_element _class "subheader"

    let create_subheader ?(classes=[]) ?attrs ?(tag=h3) ~text () =
      tag ~a:([ a_class (subheader_class :: classes) ] <@> attrs) [pcdata text]

    let create ?(classes=[]) ?attrs ~content () =
      div ~a:([ a_class (_class :: classes)] <@> attrs) content

  end

  let dense_class    = CSS.add_modifier base_class "dense"
  let two_line_class = CSS.add_modifier base_class "two-line"
  let avatar_class   = CSS.add_modifier base_class "avatar-list"

  let create ?(classes=[]) ?(tag=ul) ?attrs
        ?(avatar=false) ?(dense=false) ?(two_line=false) ~items () =
    tag ~a:([ a_class (classes
                       |> cons_if dense    dense_class
                       |> cons_if two_line two_line_class
                       |> cons_if avatar   avatar_class
                       |> List.cons base_class) ] <@> attrs) items

end
