open Utils
open Containers

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class           = "mdc-card"
  let stroked_class        = CSS.add_modifier "stroked"
  let primary_action_class = CSS.add_element "primary-action"

  module Media = struct
    let _class           = CSS.add_element base_class "media"
    let content_class    = CSS.add_element base_class "media-content"
    let square_class     = CSS.add_modifier _class "square"
    let widescreen_class = CSS.add_modifier _class "16-9"

    let create ?(classes=[]) ?attrs ~children () =
      section ~a:([a_class (_class :: classes)] <@> attrs) children
  end

  module Actions = struct
    let _class              = CSS.add_element base_class "actions"
    let full_bleed_class    = CSS.add_modifier _class "full-bleed"
    let action_class        = CSS.add_element base_class "action"
    let action_button_class = CSS.add_modifier action_class "button"
    let action_icon_class   = CSS.add_modifier action_class "icon"

    module Buttons = struct
      let _class = CSS.add_element base_class "action-buttons"

      let create ?(classes=[]) ?attrs ~children () =
        div ~a:([ a_class (_class :: classes) ] <@> attrs) children
    end

    module Icons = struct
      let _class = CSS.add_element base_class "action-icons"

      let create ?(classes=[]) ?attrs ~children () =
        div ~a:([ a_class (_class :: classes) ] <@> attrs) children
    end

    let create ?(classes=[]) ?attrs ~children () =
      section ~a:([ a_class (classes |> List.cons _class) ] <@> attrs) children
  end

  module Primary = struct

    let _class            = CSS.add_element base_class "primary"
    let title_class       = CSS.add_element base_class "title"
    let subtitle_class    = CSS.add_element base_class "subtitle"
    let overline_class    = CSS.add_element base_class "overline"
    let large_title_class = CSS.add_modifier title_class "large"

    let create_overline ?(classes=[]) ?attrs ~text () =
      h5 ~a:([a_class (overline_class :: classes)]
             <@> attrs)
        [pcdata text]

    let create_title ?(classes=[]) ?attrs ?(large=false) ~title () =
      h2 ~a:([a_class (classes
                       |> cons_if large large_title_class
                       |> List.cons title_class)]
             <@> attrs)
        [pcdata title]

    let create_subtitle ?(classes=[]) ?attrs ~subtitle () =
      h3 ~a:([a_class (subtitle_class :: classes)]
             <@> attrs)
        [pcdata subtitle]

    let create ?(classes=[]) ?attrs ~children () =
      section ~a:([a_class (_class :: classes)] <@> attrs) children

  end

  let create ?(classes=[]) ?attrs ?tag ~sections () =
    let tag = Option.get_or ~default:div tag in
    tag ~a:([a_class (base_class :: classes)] <@> attrs) sections

end
