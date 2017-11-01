open Tyxml

       [@@@ocaml.warning "-60"]

module CSS : sig

  val add_element   : string -> string -> string
  val add_elements  : string -> string list -> string
  val add_modifier  : string -> string -> string
  val add_modifiers : string -> string list -> string

end = struct

  let concat_one s e d  = s ^ d ^ e
  let concat_many s e d = String.concat d (s :: e)

  let add_element s e   = concat_one s e "__"
  let add_elements s e  = concat_many s e "__"
  let add_modifier s e  = concat_one s e "--"
  let add_modifiers s e = concat_many s e "--"

end

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
                         and module Svg := Svg) = struct
  open Html

  let add_common_attrs ?id ?style ?(attrs=[]) l =
    l
    |> (fun l -> match id with
                 | Some x -> l @ [Html.a_id x]
                 | None   -> l)
    |> (fun l -> match style with
                 | Some x -> l @ [Html.a_style x]
                 | None   -> l)
    |> (fun l -> l @ attrs)

  module Button = struct

    let base_class = "mdc-button"

    let create ?(classes=[]) ?id ?style
               ?(disabled=false) ?color_scheme ?(raised=false) ?(ripple=false)
               ?(dense=false) ?(compact=false) ?label ?onclick ?attrs () =
      button ~a:([ a_class (base_class :: classes
                            |> (fun l -> match color_scheme with
                                         | None   -> l
                                         | Some x -> match x with
                                                     | `Primary -> (CSS.add_modifier base_class "primary") :: l
                                                     | `Accent  -> (CSS.add_modifier base_class "accent") :: l)
                            |> (fun l -> if raised  then (CSS.add_modifier base_class "raised") :: l  else l)
                            |> (fun l -> if dense   then (CSS.add_modifier base_class "dense") :: l   else l)
                            |> (fun l -> if compact then (CSS.add_modifier base_class "compact") :: l else l)) ]
                 |> add_common_attrs ?id ?style ?attrs
                 |> (fun l -> match onclick with
                              | Some x -> l @ [a_onclick x]
                              | None   -> l)
                 |> (fun l -> if disabled then l @ [a_disabled ()] else l)
                 |> (fun l -> if ripple   then l @ [a_user_data "mdc-auto-init" "MDCRipple"] else l))
             (match label with
              | Some label -> [pcdata label]
              | None       -> [])
  end

  module Card = struct

    let base_class = "mdc-card"

    module Media = struct

      let _class = CSS.add_element base_class "media"

      let create ?(classes=[]) ?id ?style ?attrs ~children () =
        section ~a:([a_class (_class :: classes)]
                    |> add_common_attrs ?id ?style ?attrs)
                children

    end

    module Actions = struct

      let _class = CSS.add_element base_class "actions"

      let create ?(classes=[]) ?id ?style ?attrs ~children () =
        section ~a:([a_class (_class :: classes)]
                    |> add_common_attrs ?id ?style ?attrs)
                children

    end

    module Primary = struct

      let _class         = CSS.add_element base_class "primary"
      let title_class    = CSS.add_element base_class "title"
      let subtitle_class = CSS.add_element base_class "subtitle"

      let create_title ?(classes=[]) ?id ?style ?attrs ?(large=false) ~title () =
        h1 ~a:([a_class (title_class :: classes
                         |> (fun l -> if large then (CSS.add_modifier title_class "large") :: l else l))]
               |> add_common_attrs ?id ?style ?attrs)
           [pcdata title]

      let create_subtitle ?(classes=[]) ?id ?style ?attrs ~subtitle () =
        h2 ~a:([a_class (subtitle_class :: classes)]
               |> add_common_attrs ?id ?style ?attrs)
           [pcdata subtitle]

      let create ?(classes=[]) ?id ?style ?attrs ~children () =
        section ~a:([a_class (_class :: classes)]
                    |> add_common_attrs ?id ?style ?attrs)
                children

    end

    module Supporting_text = struct

      let _class = CSS.add_element base_class "supporting-text"

      let create ?(classes=[]) ?id ?style ?attrs ~children () =
        section ~a:([a_class (_class :: classes)]
                    |> add_common_attrs ?id ?style ?attrs)
                children

    end

    let create ?(sections=[]) ?id ?style ?(classes=[]) ?attrs () =
      div ~a:([a_class ("mdc-card" :: classes)]
              |> add_common_attrs ?id ?style ?attrs)
          sections

  end

  module Checkbox = struct

    let base_class           = "mdc-checkbox"
    let native_control_class = CSS.add_element base_class "native-control"
    let background_class     = CSS.add_element base_class "background"
    let checkmark_class      = CSS.add_element base_class "checkmark"
    let checkmark_path_class = CSS.add_element checkmark_class "path"
    let mixedmark_class      = CSS.add_element base_class "mixedmark"

    let create ?(classes=[]) ?style ?id ?input_id ?(disabled=false) ?(js=true) ?(checked=false) ?attrs () =
      div ~a:([a_class (base_class :: classes
                        |> (fun x -> if disabled then (CSS.add_modifier base_class "disabled") :: x else x))]
              |> add_common_attrs ?id ?style ?attrs
              |> (fun x -> if js then x @ [a_user_data "mdc-auto-init" "MDCCheckbox"] else x))
          [ input ~a:([ a_input_type `Checkbox
                      ; a_class [native_control_class]]
                      |> add_common_attrs ?id:input_id ?style:None
                      |> (fun x -> if disabled then a_disabled () :: x else x)
                      |> (fun x -> if checked then a_checked () :: x else x)) ()
          ; div ~a:([a_class [background_class]])
                [ svg ~a:([ Svg.a_class [checkmark_class]
                          ; Svg.a_viewBox (0.0, 0.0, 24.0, 24.0)])
                      [Svg.path ~a:([ Svg.a_class [checkmark_path_class]
                                    ; Svg.a_fill `None
                                    ; Svg.a_stroke (`Color ("white",None))
                                    ; Svg.a_d "M1.73,12.91 8.1,19.28 22.79,4.59"])
                                []]]
          ; div ~a:[a_class [mixedmark_class]] []]

  end

  module Dialog = struct

    let base_class     = "mdc-dialog"
    let surface_class  = CSS.add_element base_class "surface"
    let backdrop_class = CSS.add_element base_class "backdrop"

    module Header = struct

      let _class = CSS.add_element base_class "header"
      let title_class  = CSS.add_element _class "title"

      let create ~id ?style ?(classes=[]) ?attrs ~label () =
        header ~a:([ a_class [_class]])
               [ h2 ~a:([ a_class (title_class :: classes) ]
                        |> add_common_attrs ~id ?style ?attrs)
                    [ pcdata label ]]
    end

    module Body = struct

      let _class = CSS.add_element base_class "body"

      let create ?(scrollable=false) ~id ?style ?(classes=[]) ?attrs ~children () =
        section ~a:([ a_class ([_class]
                               |> (fun x -> if scrollable
                                            then x @ [CSS.add_modifier _class "scrollable"]
                                            else x)
                               |> (fun x -> x @ classes)) ]
                    |> add_common_attrs ~id ?style ?attrs)
                children

    end

    module Footer = struct

      let _class = CSS.add_element base_class "footer"
      let button_class = CSS.add_element _class "button"
      let accept_button_class = CSS.add_modifier button_class "accept"
      let cancel_button_class = CSS.add_modifier button_class "cancel"

      let create_button ?id ?style ?(classes=[]) ?attrs ?ripple ?(action=false) ~_type ~label () =
        Button.create ~classes:([ button_class
                                ; match _type with
                                  | `Accept  -> accept_button_class
                                  | `Decline -> cancel_button_class ]
                                |> (fun x -> if action then x @ [CSS.add_element base_class "action"] else x)
                                |> (fun x -> x @ classes))
                      ?id ?style ?attrs ?ripple ~label ()

      let create ?id ?style ?(classes=[]) ?attrs ~children () =
        footer ~a:([ a_class (_class :: classes)]
                   |> add_common_attrs ?id ?style ?attrs) children

    end

    let create ?id ?style ?(classes=[]) ?attrs ~label_id ~description_id ~content () =
      aside ~a:([ a_class (base_class :: classes)
                ; a_role ["alertdialog"]
                ; a_aria "labelledby" [label_id]
                ; a_aria "describedby" [description_id]
                ; a_user_data "mdc-auto-init" "MDCDialog"]
                |> add_common_attrs ?id ?style ?attrs)
            [ div ~a:([a_class [surface_class]]) content
            ; div ~a:([a_class [backdrop_class]]) [] ]

  end

  module Drawer = struct

  end

  module Elevation = struct

    let base_class       = "mdc-elevation"
    let transition_class = base_class ^ "-transition"

    let get_elevation_class n =
      (CSS.add_modifier base_class "z") ^ string_of_int n

  end

  module Fab = struct

    let base_class = "mdc-fab"
    let icon_class = CSS.add_element base_class "icon"

    let create ?id ?style ?(classes=[]) ?attrs ?(mini=false) ?(plain=false) ?(ripple=false) ?label ~icon () =
      button ~a:([a_class (base_class :: "material-icons" :: classes
                           |> (fun x -> if mini  then x @ [CSS.add_modifier base_class "mini"]  else x)
                           |> (fun x -> if plain then x @ [CSS.add_modifier base_class "plain"] else x))]
                 |> add_common_attrs ?id ?style ?attrs
                 |> (fun x -> if ripple then (a_user_data "mdc-auto-init" "MDCRipple") :: x else x)
                 |> (fun x -> match label with
                              | Some label -> (a_aria "label" [label]) :: x
                              | None       -> x))
             [ span ~a:[a_class [icon_class]] [pcdata icon] ]

  end

  module Form_field = struct

    let base_class = "mdc-form-field"

    module Label = struct

      let create ?id ?for_id ?style ?(classes=[]) ?attrs ~label () =
        Html.label ~a:([ a_class (base_class :: classes) ]
                       |> (fun x -> match for_id with
                                    | Some for_id -> (a_label_for for_id) :: x
                                    | None        -> x)
                       |> add_common_attrs ?id ?style ?attrs)
                   [pcdata label]

    end

    let create ?id ?style ?(classes=[]) ?attrs ?(align_end=false) ~input ~label () =
      div ~a:([ a_class (base_class :: classes
                         |> fun x -> if align_end then x @ [CSS.add_modifier base_class "align-end"] else x) ]
              |> add_common_attrs ?id ?style ?attrs)
          [ input; label ]

  end

  module Grid_list = struct

  end

  module Icon_toggle = struct

  end

  module Layout_grid = struct

  end

  module Linear_progress = struct

  end

  module List_ = struct

  end

  module Menu = struct

  end

  module Radio = struct

    let base_class           = "mdc-radio"
    let native_control_class = CSS.add_element base_class "native-control"
    let background_class     = CSS.add_element base_class "background"
    let outer_circle_class   = CSS.add_element base_class "outer-circle"
    let inner_circle_class   = CSS.add_element base_class "inner-circle"

    let create ?id ?input_id ?style ?(classes=[]) ?attrs ?(js=true) ?(checked=false) ?(disabled=false) ~name () =
      div ~a:([ a_class (base_class :: classes
                         |> fun x -> if disabled then x @ [CSS.add_modifier base_class "disabled"] else x) ]
              |> (fun x -> if js then (a_user_data "mdc-auto-init" "MDCRadio") :: x else x)
              |> add_common_attrs ?id ?style ?attrs)
          [ input ~a:([ a_class [native_control_class]
                      ; a_input_type `Radio
                      ; a_name name ]
                      |> add_common_attrs ?id:input_id ?style:None
                      |> (fun x -> if checked then a_checked () :: x else x)
                      |> (fun x -> if disabled then a_disabled () :: x else x))
                  ()
          ; div ~a:[ a_class [background_class] ]
                [ div ~a:[ a_class [outer_circle_class]] []
                ; div ~a:[ a_class [inner_circle_class]] [] ]
          ]

  end

  module Ripple = struct

  end

  module Select = struct

    let base_class = "mdc-select"

  end

  module Slider = struct

  end

  module Snackbar = struct

    let base_class           = "mdc-snackbar"
    let text_class           = CSS.add_element base_class "text"
    let action_wrapper_class = CSS.add_element base_class "action-wrapper"
    let action_button_class  = CSS.add_element base_class "action-button"

    let create ?id ?style ?(classes=[]) ?attrs ?(start_aligned=false) () =
      div ~a:([ a_class (base_class :: classes
                         |> fun x -> if start_aligned then x @ [CSS.add_modifier base_class "align-start"] else x)
              ; a_aria "live" ["assertive"]
              ; a_aria "atomic" ["true"]
              ; a_aria "hidden" ["true"]]
              |> add_common_attrs ?id ?style ?attrs)
          [ div ~a:([a_class [text_class]]) []
          ; div ~a:([a_class [action_wrapper_class]])
                [ button ~a:[ a_class [action_button_class]
                            ; a_button_type `Button ]
                         [] ]
          ]

  end

  module Switch = struct

    let base_class           = "mdc-switch"
    let native_control_class = CSS.add_element base_class "native-control"
    let background_class     = CSS.add_element base_class "background"
    let knob_class           = CSS.add_element base_class "knob"

    let create ?id ?input_id ?style ?(classes=[]) ?attrs ?(disabled=false) () =
      div ~a:([a_class (base_class :: classes
                        |> fun x -> if disabled then x @ [CSS.add_modifier base_class "disabled"] else x)]
              |> add_common_attrs ?id ?style ?attrs)
          [ input ~a:([ a_input_type `Checkbox
                      ; a_class [native_control_class]]
                      |> (fun x -> if disabled then (a_disabled ()) :: x else x)
                      |> add_common_attrs ?id:input_id ?style:None) ()
          ; div ~a:([ a_class [background_class]])
                [ div ~a:([ a_class [knob_class]]) []]
          ]

  end

  module Tabs = struct

    type tab_content = Text of string
                     | Icon of (string * string option)
                     | Text_and_icon of (string * string)

    let base_class = "mdc-tab-bar"

    let tab_class = "mdc-tab"

    let create_tab ?(classes=[]) ?id ?style ?attrs ?(active=false) ?href ~content () =
      a ~a:([a_class (tab_class :: classes
                      |> (fun l -> if active then (tab_class ^ "--active") :: l else l))]
            |> add_common_attrs ?id ?style ?attrs
            |> (fun l -> match href with
                         | Some x -> l @ [a_href x]
                         | None   -> l))
        (match content with
         | Text s -> [pcdata s]
         | Icon (i,fallback) -> [Html.i ~a:([a_class ["material-icons"; (tab_class ^ "__icon")]]
                                            |> (fun l -> match fallback with
                                                         | Some x -> l @ [a_aria "label" [x]]
                                                         | None   -> l))
                                        [pcdata i]]
         | Text_and_icon (s,i) -> [ Html.i ~a:([ a_class ["material-icons"; (tab_class ^ "__icon")]
                                               ; a_aria "hidden" ["true"]])
                                           [pcdata i]
                                  ; span ~a:[a_class [(tab_class ^ "__icon-text")]]
                                         [pcdata s]])

    let create_indicator ?(classes=[]) ?id ?style ?attrs () =
      span ~a:([a_class ((base_class ^ "__indicator") :: classes)]
               |> add_common_attrs ?id ?style ?attrs) []

    let create ?(classes=[]) ?id ?style ?attrs
               ?(with_indicator=true) ?(accent_indicator=false) ~_type ~children () =
      nav ~a:([ a_class (base_class :: classes
                         |> (fun l -> match _type with
                                      | `Text          -> l
                                      | `Icon          -> (base_class ^ "--icon-tab-bar") :: l
                                      | `Text_and_icon -> (base_class ^ "--icons-with-text") :: l)
                         |> (fun l -> if accent_indicator
                                      then (base_class ^ "--indicator-accent") :: l
                                      else l))
              ; a_user_data "mdc-auto-init" "MDCTabBar"]
              |> add_common_attrs ?id ?style ?attrs)
          (children |> (fun x -> if with_indicator then (x @ [create_indicator ()]) else x))

  end

  module Textfield = struct

    let base_class  = "mdc-textfield"
    let input_class = CSS.add_element base_class "input"

    let create ?(classes=[]) ?id ?style ?attrs ?(password=false) ?(disabled=false) ~label () =
      Html.label ~a:([ a_class (base_class :: classes)
                     ; a_user_data "mdc-auto-init" "MDCTextfield" ]
                     |> add_common_attrs ?id ?style ?attrs)
                 [ input ~a:([ a_class [input_class]
                             ; a_input_type (if password then `Password else `Text) ]
                             |> (fun l -> if disabled then l @ [a_disabled ()] else l))
                         ()
                 ; span ~a:[a_class [base_class ^ "__label"]]
                        [pcdata label]]

  end

  module Toolbar = struct

  end

  module Typography = struct

    let base_class          = "mdc-typography"
    let display4_class      = CSS.add_modifier base_class "display4"
    let display3_class      = CSS.add_modifier base_class "display3"
    let display2_class      = CSS.add_modifier base_class "display2"
    let display1_class      = CSS.add_modifier base_class "display1"
    let headline_class      = CSS.add_modifier base_class "headline"
    let title_class         = CSS.add_modifier base_class "title"
    let subheading2_class   = CSS.add_modifier base_class "subheading2"
    let subheading1_class   = CSS.add_modifier base_class "subheading1"
    let body2_class         = CSS.add_modifier base_class "body2"
    let body1_class         = CSS.add_modifier base_class "body1"
    let caption_class       = CSS.add_modifier base_class "caption"
    let button_class        = CSS.add_modifier base_class "button"
    let adjust_margin_class = CSS.add_modifier base_class "adjust-margin"

  end

end
