open Utils

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module Tab = struct
    let _class = "mdc-tab"
    let icon_class      = CSS.add_element _class "icon"
    let icon_text_class = CSS.add_element _class "icon-text"
    let active_class    = CSS.add_modifier _class "active"

    let create ?(classes=[]) ?attrs ?(active=false) ?href ~content () =
      a ~a:([ a_class (classes
                       |> cons_if active active_class
                       |> List.cons _class) ]
            |> map_cons_option a_href href
            <@> attrs)
        (match content with
         | `Text s              -> [ pcdata s ]
         | `Icon (i,fallback)   -> [ Html.i ~a:([a_class ["material-icons"; icon_class]]
                                                |> map_cons_option (fun x -> a_aria "label" [x]) fallback)
                                       [ pcdata i ] ]
         | `Text_and_icon (s,i) -> [ Html.i ~a:([ a_class ["material-icons"; icon_class]
                                                ; a_aria "hidden" ["true"]])
                                       [pcdata i]
                                   ; span ~a:[ a_class [icon_text_class] ]
                                       [ pcdata s ]])
  end

  module Tab_bar = struct

    let _class                 = "mdc-tab-bar"
    let indicator_primary_class = CSS.add_modifier _class "indicator-primary"
    let indicator_accent_class  = CSS.add_modifier _class "indicator-accent"

    module Indicator = struct
      let _class = CSS.add_element _class "indicator"

      let create ?(classes=[]) ?attrs () =
        span ~a:([a_class (_class :: classes)] <@> attrs) []
    end

    let create ?id ?style ?(classes=[]) ?attrs
          ?(indicator=Indicator.create ()) ?color_scheme ~typ ~tabs () =
      nav ~a:([ a_class (classes
                         |> (fun x -> match typ with
                                      | `Text          -> x
                                      | `Icon          -> (CSS.add_modifier _class "icon-tab-bar") :: x
                                      | `Text_and_icon -> (CSS.add_modifier _class "icons-with-text") :: x)
                         |> map_cons_option (function
                                | `Primary -> indicator_primary_class
                                | `Accent  -> indicator_accent_class)
                              color_scheme
                         |> List.cons _class) ] <@> attrs)
        (tabs @ [indicator])
  end

  module Scroller = struct
    let _class                  = "mdc-tab-bar-scroller"
    let scroll_frame_class      = CSS.add_element _class "scroll-frame"
    let scroll_frame_tabs_class = CSS.add_element scroll_frame_class "tabs"

    let indicator_class         = CSS.add_element _class "indicator"
    let indicator_enabled_class = CSS.add_modifier indicator_class "enabled"
    let indicator_inner_class   = CSS.add_element indicator_class "inner"
    let indicator_back_class    = CSS.add_modifier indicator_class "back"
    let indicator_forward_class = CSS.add_modifier indicator_class "forward"

    let create_indicator ~direction () =
      div ~a:[a_class [ indicator_class;
                        (match direction with
                         | `Back    -> indicator_back_class
                         | `Forward -> indicator_forward_class)]]
        [ a ~a:([ a_class [ indicator_inner_class; "material-icons" ]
                (* ; a_href @@ uri_of_string "#" *)
                ; a_aria "label" ["scroll"
                                ; (match direction with
                                   | `Back -> "back"
                                   | `Forward -> "forward")
                                ; "button"]])
            [pcdata (match direction with
                     | `Back    -> "navigate_before"
                     | `Forward -> "navigate_next")] ]

    let create ?(classes=[]) ?attrs ~tabs () =
      div ~a:([ a_class (_class :: classes) ] <@> attrs)
        [ div ~a:[ a_class [scroll_frame_class]]
            [ create_indicator ~direction:`Back ()
            ; div ~a:[ a_class [scroll_frame_tabs_class]
                     ; a_role ["tablist"]] [ tabs ]
            ; create_indicator ~direction:`Forward ()
            ]
        ]

  end

end
