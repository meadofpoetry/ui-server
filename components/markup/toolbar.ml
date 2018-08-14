open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class                = "mdc-toolbar"
  let fixed_adjust_class        = base_class ^ "-fixed-adjust"
  let fixed_class               = CSS.add_modifier base_class "fixed"
  let waterfall_class           = CSS.add_modifier base_class "waterfall"
  let fixed_last_row_only_class = CSS.add_modifier base_class "fixed-lastrow-only"
  let flexible_class            = CSS.add_modifier base_class "flexible"

  module Row = struct

    module Section = struct
      let _class              = CSS.add_element base_class "section"
      let title_class         = CSS.add_element base_class "title"
      let icon_class          = CSS.add_element base_class "icon"
      let menu_icon_class     = CSS.add_element base_class "menu-icon"
      let align_start_class   = CSS.add_modifier _class "align-start"
      let align_end_class     = CSS.add_modifier _class "align-end"
      let shrink_to_fit_class = CSS.add_modifier _class "shrink-to-fit"

      let create_title ?(classes=[]) ?attrs ~title () =
        span ~a:([ a_class (title_class :: classes)] <@> attrs)
          [ pcdata title ]

      let create ?(classes=[]) ?attrs ?align ?(shrink_to_fit=false) ~content () =
        section ~a:([ a_class (classes
                               |> map_cons_option (function
                                      | `Start -> align_start_class
                                      | `End   -> align_end_class) align
                               |> cons_if shrink_to_fit shrink_to_fit_class
                               |> List.cons _class) ] <@> attrs)
          content

    end

    let _class = CSS.add_element base_class "row"

    let create ?(classes=[]) ?attrs ~content () =
      div ~a:([ a_class (_class :: classes) ] <@> attrs) content
  end

  let create ?(classes=[]) ?attrs
        ?(fixed=false) ?(fixed_last_row=false) ?(waterfall=false)
        ?(flexible=false) ?flexible_height ~content () =
    let style =
      CCOpt.map (fun x -> "--" ^ base_class ^ "-ratio-to-extend-flexible: %d;" ^ (string_of_int x))
        flexible_height
    in
    header ~a:([ a_class (classes
                          |> cons_if fixed fixed_class
                          |> cons_if (fixed && waterfall) waterfall_class
                          |> cons_if (fixed && fixed_last_row) fixed_last_row_only_class
                          |> cons_if flexible flexible_class
                          |> List.cons base_class) ]
               |> map_cons_option a_style style
               <@> attrs) content

end
