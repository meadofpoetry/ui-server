open Utils
open Containers

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class        = "mdc-text-field"
  let input_class       = CSS.add_element base_class "input"
  let label_class       = "mdc-floating-label"
  let ripple_line_class = "mdc-line-ripple"

  module Wrapper = struct
    let _class = "mdc-text-field-wrapper"

    let create ?(classes=[]) ?attrs ~textfield ~helptext () =
      section ~a:([ a_class (_class :: classes) ] <@> attrs) [ textfield; helptext ]
  end

  module Notched_outline = struct
    let _class        = "mdc-notched-outline"
    let notched_class = CSS.add_modifier _class "notched"
    let path_class    = CSS.add_element _class "path"
    let idle_class    = CSS.add_element _class "idle"

    let create_main ?(classes=[]) ?attrs () =
      div ~a:([ a_class (_class :: classes) ] <@> attrs)
        [ svg ~a:([]) [Svg.path ~a:([ Svg.a_class [path_class]]) [] ]]

    let create_idle ?attrs () =
      div ~a:([a_class [idle_class]] <@> attrs) []
  end

  module Floating_label = struct
    let _class            = "mdc-floating-label"
    let float_above_class = CSS.add_modifier _class "float-above"
    let shake_class       = CSS.add_modifier _class "shake"

    let create ?(classes=[]) ?attrs ~data ~fore () =
      label ~a:([ a_class (_class :: classes) ]
                |> List.cons (a_label_for fore)
                <@> attrs) [pcdata data]
  end

  module Help_text = struct
    let _class               = "mdc-text-field-helper-text"
    let persistent_class     = CSS.add_modifier _class "persistent"
    let validation_msg_class = CSS.add_modifier _class "validation-msg"

    let create ?(classes=[]) ?attrs ?(persistent=false) ?(validation=false) ?text () =
      p ~a:([ a_class (classes
                       |> cons_if validation validation_msg_class
                       |> cons_if persistent persistent_class
                       |> List.cons _class) ]
            |> cons_if (not persistent) @@ a_aria "hidden" ["true"]
            <@> attrs) [pcdata @@ Option.get_or ~default:"" text]
  end

  module Icon = struct
    let _class = CSS.add_element base_class "icon"

    let create ?(classes=[]) ?attrs ?(clickable=true) ~icon () =
      Html.i ~a:([ a_class ("material-icons" :: _class :: classes) ]
                 |> cons_if clickable @@ a_tabindex 0
                 <@> attrs) [pcdata icon]
  end

  let textarea_class          = CSS.add_modifier base_class "textarea"
  let dense_class             = CSS.add_modifier base_class "dense"
  let fullwidth_class         = CSS.add_modifier base_class "fullwidth"
  let disabled_class          = CSS.add_modifier base_class "disabled"
  let box_class               = CSS.add_modifier base_class "box"
  let upgraded_class          = CSS.add_modifier base_class "upgraded"  (* pre-filled *)
  let outlined_class          = CSS.add_modifier base_class "outlined"
  let leading_icon_class      = CSS.add_modifier base_class "with-leading-icon"
  let trailing_icon_class     = CSS.add_modifier base_class "with-trailing-icon"
  let focused_class           = CSS.add_modifier base_class "focused"
  let label_float_above_class = CSS.add_modifier label_class "float-above"

  let create ?(classes=[]) ?attrs ?placeholder
        ?value ?(input_type=`Text) ?(disabled=false) ?label ?help_text_id
        ?leading_icon ?trailing_icon ?(box=false) ?(required=false) ?(full_width=false) ?(dense=false)
        ?(textarea=false) ?outline ?(focused=false) ?rows ?cols ~input_id () =
    div ~a:([ a_class (classes
                       |> cons_if textarea   textarea_class
                       |> cons_if full_width fullwidth_class
                       |> cons_if dense      dense_class
                       |> cons_if disabled   disabled_class
                       |> cons_if box        box_class
                       |> cons_if (Option.is_some outline) outlined_class
                       |> cons_if focused    focused_class
                       |> cons_if (Option.is_some leading_icon) leading_icon_class
                       |> cons_if (Option.is_some trailing_icon) trailing_icon_class
                       |> cons_if (Option.is_some value) upgraded_class
                       |> List.cons base_class) ]
            <@> attrs)
      ((if textarea || (Option.is_some outline)
        then []
        else [ div ~a:([ a_class [ripple_line_class]]) [] ])
       |> cons_if (Option.is_some outline) (Notched_outline.create_idle ())
       |> cons_if (Option.is_some outline) (Notched_outline.create_main ())
       |> cons_option trailing_icon
       |> cons_option label
       |> List.cons
            (let common_attrs = [ a_class [input_class] ]
                                |> cons_if required @@ a_required ()
                                |> cons_if disabled @@ a_disabled ()
                                |> map_cons_option a_placeholder placeholder
                                |> List.cons (a_id input_id)
             in
             if not textarea
             then (input ~a:(common_attrs @ [ a_input_type input_type ]
                             |> map_cons_option a_value value
                             |> map_cons_option (fun x -> a_aria "controls" [x]) help_text_id) ())
             else (Html.textarea ~a:(common_attrs
                                     |> map_cons_option a_rows rows
                                     |> map_cons_option a_cols cols)
                     (pcdata @@ Option.get_or ~default:"" value)))
       |> cons_option leading_icon)

end
