open Tyxml

       [@@@ocaml.warning "-60"]

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
                         and module Svg := Svg) = struct
  open Html

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

  let cons_option opt l =
    CCOpt.map_or ~default:l (fun x -> x :: l) opt

  let map_cons_option ~f opt l =
    CCOpt.map_or ~default:l (fun x -> (f x) :: l) opt

  let cons_if case x l =
    if case then x :: l else l

  let map_cons_if ~f case x l =
    if case then (f x) :: l else l

  let add_common_attrs ?id ?style ?(attrs=[]) l =
    map_cons_option ~f:Html.a_id id l
    |> map_cons_option ~f:Html.a_style style
    |> CCList.append attrs

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

  module Button = struct

    let base_class = "mdc-button"
    let icon_class = CSS.add_element base_class "icon"

    let create ?(classes=[]) ?id ?style
               ?(disabled=false) ?(raised=false) ?(ripple=false)
               ?(unelevated=false) ?(stroked=false) ?(dense=false) ?(compact=false)
               ?icon ?label ?onclick ?attrs () =
      button ~a:([ a_class (classes
                            |> cons_if unelevated @@ CSS.add_modifier base_class "unelevated"
                            |> cons_if stroked    @@ CSS.add_modifier base_class "stroked"
                            |> cons_if raised     @@ CSS.add_modifier base_class "raised"
                            |> cons_if dense      @@ CSS.add_modifier base_class "dense"
                            |> cons_if compact    @@ CSS.add_modifier base_class "compact"
                            |> CCList.cons base_class) ]
                 |> add_common_attrs ?id ?style ?attrs
                 |> map_cons_option ~f:a_onclick onclick
                 |> cons_if disabled @@ a_disabled ()
                 |> cons_if ripple   @@ a_user_data "mdc-auto-init" "MDCRipple")
             ((map_cons_option ~f:pcdata label [])
              |> map_cons_option ~f:(fun x -> Html.i ~a:[ a_class [icon_class;"material-icons"] ]
                                                     [pcdata x])
                                 icon)
  end

  module Card = struct

    let base_class             = "mdc-card"
    let horizontal_block_class = CSS.add_element base_class "horizontal-block"

    module Media_item = struct

      let _class              = CSS.add_element base_class "media-item"
      let height_1dot5x_class = CSS.add_modifier _class "1dot5x"
      let height_2x_class     = CSS.add_modifier _class "2x"
      let height_3x_class     = CSS.add_modifier _class "3x"

    end

    module Media = struct

      let _class = CSS.add_element base_class "media"

      let create ?(classes=[]) ?id ?style ?attrs ~children () =
        section ~a:([a_class (_class :: classes)]
                    |> add_common_attrs ?id ?style ?attrs)
                children

    end

    module Actions = struct

      let _class       = CSS.add_element base_class "actions"
      let action_class = CSS.add_element base_class "action"

      let create ?(classes=[]) ?id ?style ?attrs ?(vertical=false) ~children () =
        section ~a:([ a_class (classes
                               |> cons_if vertical @@ CSS.add_modifier _class "vertical"
                               |> CCList.cons _class) ]
                    |> add_common_attrs ?id ?style ?attrs)
                children

    end

    module Primary = struct

      let _class         = CSS.add_element base_class "primary"
      let title_class    = CSS.add_element base_class "title"
      let subtitle_class = CSS.add_element base_class "subtitle"

      let create_title ?(classes=[]) ?id ?style ?attrs ?(large=false) ~title () =
        h1 ~a:([a_class (classes
                         |> cons_if large @@ CSS.add_modifier title_class "large"
                         |> CCList.cons title_class)]
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
      div ~a:([a_class (base_class :: classes)]
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

    let create ?(classes=[]) ?style ?id ?input_id ?(disabled=false) ?(auto_init=false) ?(checked=false) ?attrs () =
      div ~a:([a_class (classes
                        |> cons_if disabled @@ CSS.add_modifier base_class "disabled"
                        |> CCList.cons base_class)]
              |> add_common_attrs ?id ?style ?attrs
              |> cons_if auto_init @@ a_user_data "mdc-auto-init" "MDCCheckbox")
          [ input ~a:([ a_input_type `Checkbox
                      ; a_class [native_control_class]]
                      |> add_common_attrs ?id:input_id ?style:None
                      |> cons_if disabled @@ a_disabled ()
                      |> cons_if checked  @@ a_checked ()) ()
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
        section ~a:([ a_class (classes
                               |> cons_if scrollable @@ CSS.add_modifier _class "scrollable"
                               |> CCList.cons _class) ]
                    |> add_common_attrs ~id ?style ?attrs)
                children

    end

    module Footer = struct

      let _class = CSS.add_element base_class "footer"
      let button_class = CSS.add_element _class "button"
      let accept_button_class = CSS.add_modifier button_class "accept"
      let cancel_button_class = CSS.add_modifier button_class "cancel"

      let create_button ?id ?style ?(classes=[]) ?attrs ?ripple ?(action=false) ~_type ~label () =
        Button.create ~classes:((match _type with
                                 | `Accept  -> accept_button_class
                                 | `Decline -> cancel_button_class) :: classes
                                |> cons_if action @@ CSS.add_element base_class "action"
                                |> CCList.cons button_class)
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

  module Elevation = struct

    let base_class       = "mdc-elevation"
    let transition_class = base_class ^ "-transition"

    let get_elevation_class n = (CSS.add_modifier base_class "z") ^ string_of_int n

  end

  module Fab = struct

    let base_class   = "mdc-fab"
    let icon_class   = CSS.add_element base_class "icon"
    let exited_class = CSS.add_modifier base_class "exited"

    let create ?id ?style ?(classes=[]) ?attrs ?onclick
               ?(mini=false) ?(ripple=false) ?label ~icon () =
      button ~a:([ a_class (classes
                            |> cons_if mini @@ CSS.add_modifier base_class "mini"
                            |> CCList.cons base_class
                            |> CCList.cons "material-icons") ]
                 |> add_common_attrs ?id ?style ?attrs
                 |> cons_if ripple @@ a_user_data "mdc-auto-init" "MDCRipple"
                 |> map_cons_option ~f:(fun x -> a_aria "label" [x]) label
                 |> map_cons_option ~f:a_onclick onclick)
             [ span ~a:[a_class [icon_class]] [pcdata icon] ]

  end

  module Form_field = struct

    let base_class = "mdc-form-field"

    module Label = struct

      let create ?id ?for_id ?style ?(classes=[]) ?attrs ~label () =
        Html.label ~a:([ a_class (base_class :: classes) ]
                       |> map_cons_option ~f:a_label_for for_id
                       |> add_common_attrs ?id ?style ?attrs)
                   [pcdata label]

    end

    let create ?id ?style ?(classes=[]) ?attrs ?(align_end=false) ~input ~label () =
      div ~a:([ a_class (classes
                         |> cons_if align_end @@ CSS.add_modifier base_class "align-end"
                         |> CCList.cons base_class) ]
              |> add_common_attrs ?id ?style ?attrs)
          [ input; label ]

  end

  module Grid_list = struct

    let base_class  = "mdc-grid-list"
    let tiles_class = CSS.add_element base_class "tiles"

    module Tile = struct

      let _class                = "mdc-grid-tile"
      let primary_class         = CSS.add_element _class "primary"
      let primary_content_class = CSS.add_element _class "primary-content"
      let secondary_class       = CSS.add_element _class "secondary"
      let icon_class            = CSS.add_element _class "icon"
      let title_class           = CSS.add_element _class "title"
      let support_text_class    = CSS.add_element _class "support-text"

      let create_primary ?id ?style ?(classes=[]) ?attrs
                         ?content_id ?content_style ?(content_classes=[]) ?content_attrs
                         ?(is_div=false) ?src ?alt () =
        div ~a:([ a_class (primary_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs)
            [ match is_div with
              | false -> img ~src:(Html.uri_of_string (CCOpt.get_or ~default:"" src))
                             ~alt:(CCOpt.get_or ~default:"" alt)
                             ~a:([ a_class (primary_content_class :: content_classes) ]
                                 |> add_common_attrs ?id:content_id ?style:content_style ?attrs:content_attrs) ()
              | true  -> div ~a:([ a_class (primary_content_class :: content_classes) ]
                                 |> add_common_attrs ?id:content_id
                                                     ?style:(match src with
                                                             | Some x ->
                                                                Printf.sprintf "background-image: url(%s);" x
                                                                |> (fun s -> Some (match content_style with
                                                                                   | Some x -> s ^ x
                                                                                   | None   -> s))
                                                             | None -> content_style)
                                                     ?attrs:content_attrs) []]

      let create_secondary
            ?id ?style ?(classes=[]) ?attrs
            ?title ?title_id ?title_style ?(title_classes=[]) ?title_attrs
            ?support_text ?support_text_id ?support_text_style ?(support_text_classes=[]) ?support_text_attrs
            ?icon () =
        span ~a:([ a_class (secondary_class :: classes) ]
                 |> add_common_attrs ?id ?style ?attrs)
             ((map_cons_option ~f:(fun x -> span ~a:([ a_class (support_text_class :: support_text_classes) ]
                                                     |> add_common_attrs ?id:support_text_id
                                                                         ?style:support_text_style
                                                                         ?attrs:support_text_attrs)
                                                 [pcdata x]) support_text [])
              |> map_cons_option ~f:(fun x -> span ~a:([ a_class (title_class :: title_classes) ]
                                                       |> add_common_attrs ?id:title_id
                                                                           ?style:title_style
                                                                           ?attrs:title_attrs)
                                                   [pcdata x])
                                 title
              |> cons_option icon)

      let create ?id ?style ?(classes=[]) ?attrs ?primary ?secondary () =
        li ~a:([ a_class (_class :: classes)]
               |> add_common_attrs ?id ?style ?attrs)
           (cons_option secondary [] |> cons_option primary )

    end

    let ar_to_string = function
      | `AR_1_1  -> "1x1" | `AR_16_9 -> "16x9" | `AR_2_3  -> "2x3"
      | `AR_3_2  -> "3x2" | `AR_4_3  -> "4x3"  | `AR_3_4  -> "3x4"

    let create ?id ?style ?(classes=[]) ?attrs
               ?ar ?(one_px_gutter=false) ?(header_caption=false) ?(twoline=false) ?icon_align
               ~tiles () =
      div ~a:([ a_class (base_class :: classes
                         |> map_cons_option ~f:(fun x -> CSS.add_modifier base_class
                                                                          ("tile-aspect-" ^ (ar_to_string x)))
                                            ar
                         |> cons_if one_px_gutter  @@ CSS.add_modifier base_class "tile-gutter-1"
                         |> cons_if header_caption @@ CSS.add_modifier base_class "header-caption"
                         |> cons_if twoline        @@ CSS.add_modifier base_class "twoline-caption"
                         |> map_cons_option ~f:(function
                                                | `Start -> CSS.add_modifier base_class "with-icon-align-start"
                                                | `End   -> CSS.add_modifier base_class "with-icon-align-end")
                                            icon_align) ]
              |> add_common_attrs ?id ?style ?attrs)
          [ ul ~a:[ a_class [tiles_class] ] tiles ]

  end

  module Icon_toggle = struct

    type data =
      { content   : string
      ; label     : string option
      ; css_class : string option [@key "cssClass"]
      } [@@deriving to_yojson]

    let base_class  = "mdc-icon-toggle"
    let icons_class = "material-icons"

    let create ?id ?style ?(classes=[]) ?attrs ?(disabled=false) ?color_scheme
               ~on_content ?on_label ?on_class
               ~off_content ?off_label ?off_class
               () =
      let data_toggle_on = { content   = on_content
                           ; label     = on_label
                           ; css_class = on_class
                           }
                           |> data_to_yojson
                           |> Yojson.Safe.to_string in
      let data_toggle_off = { content   = off_content
                            ; label     = off_label
                            ; css_class = off_class
                            }
                            |> data_to_yojson
                            |> Yojson.Safe.to_string in
      i ~a:([ a_class (classes
                       |> cons_if disabled @@ CSS.add_modifier base_class "disabled"
                       |> map_cons_option ~f:(function
                                              | `Primary -> CSS.add_modifier base_class "primary"
                                              | `Accent  -> CSS.add_modifier base_class "accent")
                                          color_scheme
                       |> CCList.cons base_class
                       |> CCList.cons icons_class)
            ; a_role ["button"]
            ; a_user_data "toggle-on"  data_toggle_on
            ; a_user_data "toggle-off" data_toggle_off
            ; a_aria "pressed" ["false"]
            ; a_tabindex (if disabled then -1 else 0) ]
            |> map_cons_option ~f:(fun x -> a_aria "label" [x]) off_label
            |> cons_if disabled @@ a_aria "disabled" ["true"]
            |> add_common_attrs ?id ?style ?attrs)
        [pcdata off_content]

  end

  module Layout_grid = struct

    let max_columns = 12

    let check_columns_number_exn n =
      if n > max_columns || n < 0 then failwith "Layout grid: bad columns number"

    let base_class  = "mdc-layout-grid"
    let inner_class = CSS.add_element base_class "inner"

    let get_grid_align position =
      CSS.add_modifier base_class ("align-" ^ (match position with
                                               | `Left  -> "left"
                                               | `Right -> "right"))

    module Cell = struct

      type span =
        { columns     : int
        ; device_type : [ `Desktop | `Tablet | `Phone ] option
        }

      let _class  = CSS.add_element base_class "cell"

      let get_cell_span ?device_type n =
        check_columns_number_exn n;
        CSS.add_modifier _class ("span-" ^ (string_of_int n))
        |> (fun s -> match device_type with
                     | Some dt -> (match dt with
                                   | `Desktop -> s ^ "-desktop"
                                   | `Tablet  -> s ^ "-tablet"
                                   | `Phone   -> s ^ "-phone")
                     | None    -> s)

      let get_cell_order n =
        check_columns_number_exn n;
        CSS.add_modifier _class ("order" ^ (string_of_int n))

      let get_cell_align align =
        CSS.add_modifier _class ("align-" ^ (match align with
                                             | `Top    -> "top"
                                             | `Middle -> "middle"
                                             | `Bottom -> "bottom"))

      let create ?id ?style ?(classes=[]) ?attrs ?span ?align ?order ~content () =
        div ~a:([ a_class (_class :: classes
                           |> map_cons_option ~f:(fun x -> get_cell_span ?device_type:x.device_type
                                                                         x.columns)
                                              span
                           |> map_cons_option ~f:get_cell_align align
                           |> map_cons_option ~f:get_cell_order order) ]
                |> add_common_attrs ?id ?style ?attrs)
            content

    end

    let create_inner ?id ?style ?(classes=[]) ?attrs ~cells () =
      div ~a:([ a_class (inner_class :: classes)]
              |> add_common_attrs ?id ?style ?attrs)
          cells

    let create ?id ?style ?(classes=[]) ?attrs ?align ?(fixed_column_width=false) ~content () =
      div ~a:([ a_class (classes
                         |> map_cons_option ~f:get_grid_align align
                         |> cons_if fixed_column_width @@ CSS.add_modifier base_class "fixed-column-width"
                         |> CCList.cons base_class) ]
              |> add_common_attrs ?id ?style ?attrs)
          content

  end

  module Linear_progress = struct

    let base_class           = "mdc-linear-progress"
    let buffering_dots_class = CSS.add_element base_class "buffering-dots"
    let buffer_class         = CSS.add_element base_class "buffer"
    let bar_class            = CSS.add_element base_class "bar"
    let primary_bar_class    = CSS.add_element base_class "primary-bar"
    let secondary_bar_class  = CSS.add_element base_class "secondary-bar"
    let inner_bar_class      = CSS.add_element base_class "bar-inner"

    let create ?id ?style ?(classes=[]) ?attrs
               ?buffering_dots_id ?buffering_dots_style ?(buffering_dots_classes=[]) ?buffering_dots_attrs
               ?buffer_id ?buffer_style ?(buffer_classes=[]) ?buffer_attrs
               ?primary_bar_id ?primary_bar_style ?(primary_bar_classes=[]) ?primary_bar_attrs
               ?secondary_bar_id ?secondary_bar_style ?(secondary_bar_classes=[]) ?secondary_bar_attrs
               ?(indeterminate=false) ?(reversed=false) ?(accent=false) () =
      div ~a:([ a_role ["progressbar"]
              ; a_class (classes
                         |> cons_if indeterminate @@ CSS.add_modifier base_class "indeterminate"
                         |> cons_if reversed      @@ CSS.add_modifier base_class "reversed"
                         |> cons_if accent        @@ CSS.add_modifier base_class "accent"
                         |> CCList.cons base_class) ]
              |> add_common_attrs ?id ?style ?attrs)
          [ div ~a:([ a_class (buffering_dots_class :: buffering_dots_classes) ]
                    |> add_common_attrs ?id:buffering_dots_id
                                        ?style:buffering_dots_style
                                        ?attrs:buffering_dots_attrs) []
          ; div ~a:([ a_class (buffer_class :: buffer_classes) ]
                    |> add_common_attrs ?id:buffer_id
                                        ?style:buffer_style
                                        ?attrs:buffer_attrs) []
          ; div ~a:([ a_class (bar_class :: primary_bar_class :: primary_bar_classes) ]
                    |> add_common_attrs ?id:primary_bar_id
                                        ?style:primary_bar_style
                                        ?attrs:primary_bar_attrs)
                [ span ~a:[ a_class [inner_bar_class]] [] ]
          ; div ~a:([ a_class (bar_class :: secondary_bar_class :: secondary_bar_classes) ]
                    |> add_common_attrs ?id:secondary_bar_id
                                        ?style:secondary_bar_style
                                        ?attrs:secondary_bar_attrs)
                [ span ~a:[ a_class [inner_bar_class]] [] ]
          ]

  end

  module List_ = struct

    let base_class   = "mdc-list"
    let avatar_class = CSS.add_element base_class "avatar-list"

    module Item = struct

      let _class               = "mdc-list-item"
      let text_class           = CSS.add_element _class "text"
      let secondary_text_class = CSS.add_element text_class "secondary"
      let start_detail_class   = CSS.add_element _class "start-detail"
      let end_detail_class     = CSS.add_element _class "end-detail"
      let divider_class        = "mdc-list-divider"

      let create_divider ?id ?style ?(classes=[]) ?attrs ?(tag=div) ?(inset=false) () =
        tag ~a:([ a_class (classes
                           |> cons_if inset @@ CSS.add_modifier divider_class "inset"
                           |> CCList.cons divider_class)
                ; a_role ["separator"] ]
                |> add_common_attrs ?id ?style ?attrs)
            []

      let create ?id ?style ?(classes=[]) ?attrs ?(auto_init=false) ?(tag=div)
                 ~text ?text_id ?text_style ?(text_classes=[]) ?text_attrs ?secondary_text
                 ?secondary_text_id ?secondary_text_style ?(secondary_text_classes=[]) ?secondary_text_attrs
                 ?start_detail ?end_detail () =
        tag ~a:([ a_class (_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs
                |> cons_if auto_init @@ a_user_data "mdc-auto-init" "MDCRipple")
            ((match secondary_text with
              | Some x -> span ~a:([ a_class (text_class :: text_classes) ]
                                   |> add_common_attrs ?id:text_id ?style:text_style ?attrs:text_attrs)
                               [ pcdata text
                               ; span ~a:([ a_class (secondary_text_class :: secondary_text_classes) ]
                                          |> add_common_attrs ?id:secondary_text_id
                                                              ?style:secondary_text_style
                                                              ?attrs:secondary_text_attrs)
                                      [pcdata x]]
              | None   -> pcdata text)
             :: cons_option end_detail []
             |> cons_option start_detail)

    end

    module List_group = struct

      let _class          = "mdc-list-group"
      let subheader_class = CSS.add_element _class "subheader"

      let create_subheader ?id ?style ?(classes=[]) ?attrs ?(tag=h3) ~text () =
        tag ~a:([ a_class (subheader_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs)
            [pcdata text]

      let create ?id ?style ?(classes=[]) ?attrs ~content () =
        div ~a:([ a_class (_class :: classes)]
                |> add_common_attrs ?id ?style ?attrs)
            content

    end

    let create ?id ?style ?(classes=[]) ?attrs ?(tag=div) ?(avatar=false)
               ?(dense=false) ?(two_line=false) ~items () =
      tag ~a:([ a_class (classes
                         |> cons_if dense    @@ CSS.add_modifier base_class "dense"
                         |> cons_if two_line @@ CSS.add_modifier base_class "two-line"
                         |> cons_if avatar   @@ CSS.add_modifier base_class "avatar-list"
                         |> CCList.cons base_class) ]
              |> add_common_attrs ?id ?style ?attrs)
          items

  end

  module Drawer = struct

    module type Base = sig val base_class : string end

    module Make_item (M : Base) = struct

      let _class         = List_.Item._class
      let selected_class = CSS.add_modifier M.base_class "selected"

      let create_icon ?id ?style ?(classes=[]) ?attrs ~icon () =
        Html.i ~a:([ a_class ("material-icons" :: List_.Item.start_detail_class :: classes) ]
                   |> add_common_attrs ?id ?style ?attrs)
               [pcdata icon]

      let create ?id ?style ?(classes=[]) ?attrs ?(selected=false) ?start_detail ?href ~text () =
        Html.a ~a:([ a_class (classes
                              |> cons_if selected selected_class
                              |> CCList.cons _class)
                   ; a_aria "hidden" ["true"] ]
                   |> map_cons_option ~f:(fun x -> a_href @@ uri_of_string x) href
                   |> add_common_attrs ?id ?style ?attrs)
               (cons_option start_detail [ pcdata text ])

    end

    module Make_toolbar_spacer (M : Base) = struct

      let _class = CSS.add_element M.base_class "toolbar-spacer"

      let create ?id ?style ?(classes=[]) ?attrs ~content () =
        div ~a:([ a_class (_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs)
            content

    end

    module Make_header (M : Base) = struct

      let _class               = CSS.add_element M.base_class "header"
      let header_content_class = CSS.add_element M.base_class "header-content"

      let create ?id ?style ?(classes=[]) ?attrs ~content () =
        header ~a:([ a_class (_class :: classes) ]
                   |> add_common_attrs ?id ?style ?attrs)
               [ div ~a:([ a_class [header_content_class] ])
                     content]

    end

    module Permanent = struct

      let base_class    = "mdc-permanent-drawer"

      module Header         = Make_header(struct let base_class = base_class end)
      module Toolbar_spacer = Make_toolbar_spacer(struct let base_class = base_class end)
      module Item           = Make_item(struct let base_class = base_class end)

      let create ?id ?style ?(classes=[]) ?attrs ~content () =
        nav ~a:([ a_class (classes
                           |> CCList.cons Typography.base_class
                           |> CCList.cons base_class) ]
                |> add_common_attrs ?id ?style ?attrs)
            content

    end

    module Persistent = struct

      let base_class    = "mdc-persistent-drawer"
      let drawer_class  = CSS.add_element base_class "drawer"
      let content_class = CSS.add_element base_class "content"

      module Header         = Make_header(struct let base_class = base_class end)
      module Toolbar_spacer = Make_toolbar_spacer(struct let base_class = base_class end)
      module Item           = Make_item(struct let base_class = base_class end)

      let create ?id ?style ?(classes=[]) ?attrs ~content () =
        aside ~a:([ a_class (classes
                             |> CCList.cons Typography.base_class
                             |> CCList.cons base_class) ]
                  |> add_common_attrs ?id ?style ?attrs)
              [ nav ~a:([ a_class [drawer_class] ])
                    content
              ]

    end

    module Temporary = struct

      let base_class    = "mdc-temporary-drawer"
      let drawer_class  = CSS.add_element base_class "drawer"
      let content_class = CSS.add_element base_class "content"

      module Header         = Make_header(struct let base_class = base_class end)
      module Toolbar_spacer = Make_toolbar_spacer(struct let base_class = base_class end)
      module Item           = Make_item(struct let base_class = base_class end)

      let create ?id ?style ?(classes=[]) ?attrs ~content () =
        aside ~a:([ a_class (classes
                             |> CCList.cons Typography.base_class
                             |> CCList.cons base_class) ]
                  |> add_common_attrs ?id ?style ?attrs)
              [ nav ~a:([ a_class [drawer_class] ])
                    content
              ]

    end

  end

  module Menu = struct

    let base_class   = "mdc-simple-menu"
    let items_class  = CSS.add_element base_class "items"
    let anchor_class = "mdc-menu-anchor"

    module Item = struct

      include List_.Item

      let create ?id ?style ?classes ?attrs
                 ~text ?text_id ?text_style ?text_classes ?text_attrs ?secondary_text
                 ?secondary_text_id ?secondary_text_style ?secondary_text_classes ?secondary_text_attrs
                 ?(disabled=false) ?start_detail ?end_detail () =
        create ?id ?style ?classes
               ~attrs:([ a_role ["menuitem"] ]
                       |> cons_if disabled @@ a_aria "disabled" ["true"]
                       |> cons_if disabled @@ a_tabindex (-1)
                       |> cons_if (not disabled) @@ a_tabindex 0
                       |> (fun x -> x @ (CCOpt.get_or ~default:[] attrs)))
               ~text ?text_id ?text_style ?text_classes ?text_attrs ?secondary_text
               ?secondary_text_id ?secondary_text_style ?secondary_text_classes ?secondary_text_attrs
               ?start_detail ?end_detail ()

    end

    let create ?id ?style ?(classes=[]) ?attrs
               ?list_id ?list_style ?list_classes ?list_attrs
               ?(opened=false) ?open_from ~items () =
      div ~a:([ a_class (classes
                         |> cons_if opened @@ CSS.add_modifier base_class "open"
                         |> map_cons_option ~f:(fun x ->
                                              (match x with
                                               | `Top_left     -> "top-left"
                                               | `Top_right    -> "top-right"
                                               | `Bottom_left  -> "bottom-left"
                                               | `Bottom_right -> "bottom-right")
                                              |> (fun s -> CSS.add_modifier base_class ("open-from-" ^ s)))
                                            open_from
                         |> CCList.cons base_class)
              ; a_tabindex (-1) ]
              |> add_common_attrs ?id ?style ?attrs)
          [ List_.create ?id:list_id
                         ?style:list_style
                         ~classes:([items_class]
                                   |> (fun x -> x @ (CCOpt.get_or ~default:[] list_classes)))
                         ~attrs:([ a_role ["menu"]
                                 ; a_aria "hidden" ["true"] ]
                                 |> (fun x -> x @ (CCOpt.get_or ~default:[] list_attrs)))
                         ~items ()]

  end

  module Radio = struct

    let base_class           = "mdc-radio"
    let native_control_class = CSS.add_element base_class "native-control"
    let background_class     = CSS.add_element base_class "background"
    let outer_circle_class   = CSS.add_element base_class "outer-circle"
    let inner_circle_class   = CSS.add_element base_class "inner-circle"

    let create ?id ?input_id ?style ?(classes=[]) ?attrs ?(auto_init=true)
               ?(checked=false) ?(disabled=false) ~name () =
      div ~a:([ a_class (classes
                         |> cons_if disabled @@ CSS.add_modifier base_class "disabled"
                         |> CCList.cons base_class) ]
              |> cons_if auto_init @@ a_user_data "mdc-auto-init" "MDCRadio"
              |> add_common_attrs ?id ?style ?attrs)
          [ input ~a:([ a_class [native_control_class]
                      ; a_input_type `Radio
                      ; a_name name ]
                      |> cons_if checked @@ a_checked ()
                      |> cons_if disabled @@ a_disabled ()
                      |> add_common_attrs ?id:input_id)
                  ()
          ; div ~a:[ a_class [background_class] ]
                [ div ~a:[ a_class [outer_circle_class]] []
                ; div ~a:[ a_class [inner_circle_class]] [] ]
          ]

  end

  module Ripple = struct

    let base_class      = "mdc-ripple-surface"
    let primary_class   = CSS.add_modifier base_class "primary"
    let accent_class    = CSS.add_modifier base_class "accent"
    let unbounded_attr  = a_user_data "mdc-ripple-is-unbounded" ""

  end

  module Rtl = struct

    let ltr_attr = a_dir `Ltr
    let rtl_attr = a_dir `Rtl

  end

  module Select = struct

    let base_class          = "mdc-select"

    module Base = struct

      let menu_class          = CSS.add_element base_class "menu"
      let selected_text_class = CSS.add_element base_class "selected-text"

      module Item = struct

        include List_.Item

        let create ?id ?style ?classes ?attrs ?(disabled=false) ?(selected=false)
                   ~text ?text_id ?text_style ?text_classes ?text_attrs
                   ?start_detail ?end_detail () =
          create ?id ?style ?classes
                 ~attrs:([ a_role ["option"]]
                         |> cons_if selected @@ a_aria "selected" []
                         |> cons_if disabled @@ a_aria "disabled" ["true"]
                         |> cons_if disabled @@ a_tabindex (-1)
                         |> cons_if (not disabled) @@ a_tabindex 0
                         |> (fun x -> x @ (CCOpt.get_or ~default:[] attrs)))
                 ~text ?text_id ?text_style ?text_classes ?text_attrs ?start_detail ?end_detail ()

      end

      let create ?id ?style ?(classes=[]) ?attrs ?(selected_text="") ?(disabled=false)
                 ?menu_id ?menu_style ?(menu_classes=[]) ?menu_attrs ~items () =
        div ~a:([ a_class (classes
                           |> cons_if disabled @@ CSS.add_modifier base_class "disabled"
                           |> CCList.cons base_class)
                ; a_role ["listbox"]]
                |> cons_if (not disabled) @@ a_tabindex 0
                |> cons_if disabled       @@ a_tabindex (-1)
                |> cons_if disabled       @@ a_aria "disabled" ["true"]
                |> add_common_attrs ?id ?style ?attrs)
            [ span ~a:([ a_class [selected_text_class] ]) [pcdata selected_text]
            ; div  ~a:([ a_class (Menu.base_class :: menu_class :: menu_classes) ]
                       |> add_common_attrs ?id:menu_id ?style:menu_style ?attrs:menu_attrs)
                   [ List_.create ~classes:[Menu.items_class] ~items ()]
            ]

    end

    module Pure = struct

      module Item = struct

        let create ?id ?style ?(classes=[]) ?attrs ?(disabled=false) ?(selected=false) ?value ~text () =
          option ~a:([ a_class classes ]
                     |> map_cons_option ~f:a_value value
                     |> cons_if disabled @@ a_disabled ()
                     |> cons_if selected @@ a_selected ()
                     |> add_common_attrs ?id ?style ?attrs)
                 (pcdata text)

        let create_group ?id ?style ?(classes=[]) ?attrs ~label ~items () =
          optgroup ~a:([ a_class classes ]
                       |> add_common_attrs ?id ?style ?attrs)
                   ~label
                   items

      end

      let create ?id ?style ?(classes=[]) ?attrs ?(disabled=false) ~items () =
        select ~a:([ a_class (base_class :: classes) ]
                   |> cons_if disabled @@ a_disabled ()
                   |> add_common_attrs ?id ?style ?attrs)
               items

    end

    module Multi = struct

      let _class = "mdc-multi-select"

      module Item = struct

        let create ?id ?style ?(classes=[]) ?attrs ?(disabled=false) ?(selected=false) ?value ~text () =
          option ~a:([ a_class (List_.Item._class :: classes) ]
                     |> map_cons_option ~f:a_value value
                     |> cons_if disabled @@ a_disabled ()
                     |> cons_if selected @@ a_selected ()
                     |> add_common_attrs ?id ?style ?attrs)
                 (pcdata text)

        let create_divider ?id ?style ?(classes=[]) ?attrs () =
          option ~a:([ a_class (List_.Item.divider_class :: classes)
                     ; a_role ["presentation"]
                     ; a_disabled () ]
                     |> add_common_attrs ?id ?style ?attrs)
                 (pcdata "")

        let create_group ?id ?style ?(classes=[]) ?attrs ~label ~items () =
          optgroup ~a:([ a_class (List_.List_group._class :: classes) ]
                       |> add_common_attrs ?id ?style ?attrs)
                   ~label
                   items

      end

      let create ?id ?style ?(classes=[]) ?attrs ?(disabled=false) ?size ~items () =
        select ~a:([ a_class (_class :: List_.base_class :: classes)
                   ; a_multiple () ]
                   |> map_cons_option ~f:a_size size
                   |> cons_if disabled @@ a_disabled ()
                   |> add_common_attrs ?id ?style ?attrs)
               items

    end

  end

  module Slider = struct

    let base_class                   = "mdc-slider"
    let track_container_class        = CSS.add_element base_class "track-container"
    let track_class                  = CSS.add_element base_class "track"
    let track_marker_container_class = CSS.add_element base_class "track-marker-container"
    let thumb_container_class        = CSS.add_element base_class "thumb-container"
    let thumb_class                  = CSS.add_element base_class "thumb"
    let focus_ring_class             = CSS.add_element base_class "focus-ring"
    let pin_class                    = CSS.add_element base_class "pin"
    let pin_value_marker_class       = CSS.add_element base_class "pin-value-marker"

    let create ?id ?style ?(classes=[]) ?attrs ?(discrete=false) ?(markers=false) ?(disabled=false)
               ?label ?step ?(min=0) ?(max=100) ?(value=0) () =
      div ~a:([ a_class (classes
                         |> cons_if discrete @@ CSS.add_modifier base_class "discrete"
                         |> cons_if (discrete && markers) @@ CSS.add_modifier base_class "display-markers"
                         |> CCList.cons base_class)
              ; a_tabindex 0
              ; a_role ["slider"]
              ; a_aria "valuemin" [ string_of_int min ]
              ; a_aria "valuemax" [ string_of_int max ]
              ; a_aria "valuenow" [ string_of_int value ] ]
              |> map_cons_option ~f:(fun x -> a_aria "label" [x]) label
              |> map_cons_option ~f:(fun x -> a_user_data "step" (string_of_int x)) step
              |> cons_if disabled @@ a_aria "disabled" ["true"]
              |> add_common_attrs ?id ?style ?attrs)
          [ div ~a:([ a_class [track_container_class]])
                (cons_if (discrete && markers) (div ~a:[ a_class [track_marker_container_class]] []) []
                 |> CCList.cons @@ div ~a:([ a_class [track_class]]) [])
          ; div ~a:([ a_class [thumb_container_class]])
                ([ svg ~a:([ Svg.a_class [thumb_class]
                           ; Svg.a_width (21., None)
                           ; Svg.a_height (21., None)])
                       [ Svg.circle ~a:[ Svg.a_cx (10.5, None)
                                       ; Svg.a_cy (10.5, None)
                                       ; Svg.a_r (7.875, None)] []]
                 ; div ~a:([ a_class [focus_ring_class]]) []]
                 |> cons_if discrete (div ~a:[a_class [pin_class]]
                                          [span ~a:[a_class [pin_value_marker_class]] []]))
          ]

  end

  module Snackbar = struct

    let base_class           = "mdc-snackbar"
    let text_class           = CSS.add_element base_class "text"
    let action_wrapper_class = CSS.add_element base_class "action-wrapper"
    let action_button_class  = CSS.add_element base_class "action-button"

    let create ?id ?style ?(classes=[]) ?attrs ?(start_aligned=false) () =
      div ~a:([ a_class (classes
                         |> cons_if start_aligned @@ CSS.add_modifier base_class "align-start"
                         |> CCList.cons base_class)
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
      div ~a:([ a_class (classes
                         |> cons_if disabled @@ CSS.add_modifier base_class "disabled"
                         |> CCList.cons base_class) ]
              |> add_common_attrs ?id ?style ?attrs)
          [ input ~a:([ a_input_type `Checkbox
                      ; a_class [native_control_class]]
                      |> cons_if disabled @@ a_disabled ()
                      |> add_common_attrs ?id:input_id) ()
          ; div ~a:([ a_class [background_class]]) [ div ~a:([ a_class [knob_class]]) []] ]

  end

  module Tabs = struct

    module Tab = struct

      let _class = "mdc-tab"
      let icon_class      = CSS.add_element _class "icon"
      let icon_text_class = CSS.add_element _class "icon-text"

      let create ?id ?style ?(classes=[]) ?attrs ?(active=false) ?href ~content () =
        a ~a:([ a_class (classes
                         |> cons_if active @@ CSS.add_modifier _class "active"
                         |> CCList.cons _class) ]
              |> map_cons_option ~f:a_href href
              |> add_common_attrs ?id ?style ?attrs)
          (match content with
           | `Text s              -> [ pcdata s ]
           | `Icon (i,fallback)   -> [ Html.i ~a:([a_class ["material-icons"; icon_class]]
                                                  |> map_cons_option ~f:(fun x -> a_aria "label" [x]) fallback)
                                              [ pcdata i ] ]
           | `Text_and_icon (s,i) -> [ Html.i ~a:([ a_class ["material-icons"; icon_class]
                                                  ; a_aria "hidden" ["true"]])
                                              [pcdata i]
                                     ; span ~a:[ a_class [icon_text_class] ]
                                            [ pcdata s ]])

    end

    module Tab_bar = struct

      let _class = "mdc-tab-bar"

      module Indicator = struct

        let _class = CSS.add_element _class "indicator"

        let create ?id ?style ?(classes=[]) ?attrs () =
          span ~a:([a_class (_class :: classes)]
                   |> add_common_attrs ?id ?style ?attrs) []

      end

      let create ?id ?style ?(classes=[]) ?attrs
                 ?(indicator=Indicator.create ()) ?color_scheme ~_type ~content () =
        nav ~a:([ a_class (classes
                           |> (fun x -> match _type with
                                        | `Text          -> x
                                        | `Icon          -> (CSS.add_modifier _class "icon-tab-bar") :: x
                                        | `Text_and_icon -> (CSS.add_modifier _class "icons-with-text") :: x)
                           |> map_cons_option ~f:(function
                                                  | `Primary -> CSS.add_modifier _class "indicator-primary"
                                                  | `Accent  -> CSS.add_modifier _class "indicator-accent")
                                              color_scheme
                           |> CCList.cons _class) ]
                |> add_common_attrs ?id ?style ?attrs)
            (content @ [indicator])

    end

    module Scroller = struct

      let _class                  = "mdc-tab-bar-scroller"
      let indicator_class         = CSS.add_element _class "indicator"
      let indicator_back_class    = CSS.add_modifier indicator_class "back"
      let indicator_forward_class = CSS.add_modifier indicator_class "forward"
      let indicator_inner_class   = CSS.add_element indicator_class "inner"
      let scroll_frame_class      = CSS.add_element _class "scroll-frame"
      let scroll_frame_tabs_class = CSS.add_element scroll_frame_class "tabs"

      let create_indicator ~direction () =
        div ~a:[a_class [ indicator_class;
                          (match direction with
                           | `Back    -> indicator_back_class
                           | `Forward -> indicator_forward_class)]]
            [ a ~a:([ a_class [ indicator_inner_class; "material-icons" ]
                    ; a_href @@ uri_of_string "#"
                    ; a_aria "label" ["scroll"
                                     ; (match direction with
                                        | `Back -> "back"
                                        | `Forward -> "forward")
                                     ; "button"]])
                [pcdata (match direction with
                         | `Back    -> "navigate_before"
                         | `Forward -> "navigate_next")] ]

      let create ?id ?style ?(classes=[]) ?attrs ~tabs () =
        div ~a:([ a_class (_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs)
            [ create_indicator ~direction:`Back ()
            ; div ~a:[ a_class [scroll_frame_class] ] [ tabs ]
            ; create_indicator ~direction:`Forward ()
            ]

    end

  end

  module Textfield = struct

    let base_class        = "mdc-text-field"
    let input_class       = CSS.add_element base_class "input"
    let label_class       = CSS.add_element base_class "label"
    let bottom_line_class = CSS.add_element base_class "bottom-line"

    module Help_text = struct

      let _class = "mdc-text-field-helper-text"

      let create ?id ?style ?(classes=[]) ?attrs ?(persistent=false) ?(validation=false) ~text () =
        p ~a:([ a_class (classes
                         |> cons_if validation @@ CSS.add_modifier _class "validation-msg"
                         |> cons_if persistent @@ CSS.add_modifier _class "persistent"
                         |> CCList.cons _class) ]
              |> cons_if (not persistent) @@ a_aria "hidden" ["true"]
              |> add_common_attrs ?id ?style ?attrs)
          [pcdata text]

    end

    module Icon = struct

      let _class = CSS.add_element base_class "icon"

      let create ?id ?style ?(classes=[]) ?attrs ?(unclickable=false) ~icon () =
        Html.i ~a:([ a_class ("material-icons" :: _class :: classes) ]
                   |> cons_if (not unclickable) @@ a_tabindex 0
                   |> add_common_attrs ?id ?style ?attrs)
               [pcdata icon]

    end

    let create ?id ?style ?(classes=[]) ?attrs ?placeholder
               ?label_id ?label_style ?(label_classes=[]) ?label_attrs
               ?value ?(input_type=`Text) ?(disabled=false) ?label ?input_id ?help_text_id
               ?leading_icon ?trailing_icon ?(box=false) ?(required=false) ?(full_width=false) ?(dense=false)
               ?(textarea=false) ?rows ?cols () =
      div ~a:([ a_class (classes
                         |> cons_if textarea   @@ CSS.add_modifier base_class "textarea"
                         |> cons_if full_width @@ CSS.add_modifier base_class "fullwidth"
                         |> cons_if dense      @@ CSS.add_modifier base_class "dense"
                         |> cons_if disabled   @@ CSS.add_modifier base_class "disabled"
                         |> cons_if box        @@ CSS.add_modifier base_class "box"
                         |> cons_if (CCOpt.is_some leading_icon) (CSS.add_modifier base_class "with-leading-icon")
                         |> cons_if (CCOpt.is_some trailing_icon) (CSS.add_modifier base_class "with-trailing-icon")
                         |> cons_if (CCOpt.is_some value) (CSS.add_modifier base_class "upgraded")
                         |> CCList.cons base_class) ]
              |> add_common_attrs ?id ?style ?attrs)
          ((if textarea then [] else [ div ~a:([ a_class [bottom_line_class]]) [] ])
           |> cons_option trailing_icon
           |> map_cons_option ~f:(fun x ->
                                (Html.label ~a:([ a_class (label_classes
                                                           |> cons_if (CCOpt.is_some value)
                                                                      (CSS.add_modifier label_class "float-above")
                                                           |> CCList.cons label_class) ]
                                                |> map_cons_option ~f:a_label_for input_id
                                                |> add_common_attrs ?id:label_id
                                                                    ?style:label_style
                                                                    ?attrs:label_attrs)
                                            [pcdata x]))
                              label
           |> CCList.cons (let common_attrs = ([ a_class [input_class] ]
                                               |> cons_if required @@ a_required ()
                                               |> cons_if disabled @@ a_disabled ()
                                               |> map_cons_option ~f:a_placeholder placeholder
                                               |> map_cons_option ~f:a_id input_id) in
                           (if not textarea
                            then (input ~a:(common_attrs @ [ a_input_type input_type ]
                                            |> map_cons_option ~f:a_value value
                                            |> map_cons_option ~f:(fun x -> a_aria "controls" [x]) help_text_id) ())
                            else (Html.textarea ~a:(common_attrs
                                                    |> map_cons_option ~f:a_rows rows
                                                    |> map_cons_option ~f:a_cols cols)
                                                (pcdata @@ CCOpt.get_or ~default:"" value))))
           |> cons_option leading_icon)

  end

  module Toolbar = struct

    let base_class         = "mdc-toolbar"
    let fixed_adjust_class = base_class ^ "-fixed-adjust"

    module Row = struct

      module Section = struct

        let _class = CSS.add_element base_class "section"
        let title_class        = CSS.add_element base_class "title"
        let icon_class         = CSS.add_element base_class "icon"
        let menu_icon_class    = CSS.add_element base_class "menu-icon"

        let create_title ?id ?style ?(classes=[]) ?attrs ~title () =
          span ~a:([ a_class (title_class :: classes)]
                   |> add_common_attrs ?id ?style ?attrs)
               [ pcdata title ]

        let create ?id ?style ?(classes=[]) ?attrs ?align ?(shrink_to_fit=false) ~content () =
          section ~a:([ a_class (classes
                                 |> map_cons_option ~f:(function
                                                        | `Start -> CSS.add_modifier _class "align-start"
                                                        | `End   -> CSS.add_modifier _class "align-end") align
                                 |> cons_if shrink_to_fit @@ CSS.add_modifier _class "shrink-to-fit"
                                 |> CCList.cons _class) ]
                      |> add_common_attrs ?id ?style ?attrs)
                  content

      end

      let _class = CSS.add_element base_class "row"

      let create ?id ?style ?(classes=[]) ?attrs ~content () =
        div ~a:([ a_class (_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs)
            content

    end

    let create ?id ?style ?(classes=[]) ?attrs
               ?(fixed=false) ?(fixed_last_row=false) ?(waterfall=false)
               ?(flexible=false) ?flexible_height ~content () =
      header ~a:([ a_class (classes
                            |> cons_if fixed @@ CSS.add_modifier base_class "fixed"
                            |> cons_if (fixed && waterfall) @@ CSS.add_modifier base_class "waterfall"
                            |> cons_if (fixed && fixed_last_row) @@ CSS.add_modifier base_class "fixed-lastrow-only"
                            |> cons_if flexible @@ CSS.add_modifier base_class "flexible"
                            |> CCList.cons base_class) ]
                 |> add_common_attrs ?id
                                     ?style:(let s = "--" ^ base_class ^ "-ratio-to-extend-flexible" in
                                             match flexible_height with
                                             | Some x -> let fh_style = Printf.sprintf "%s: %d;" s x in
                                                         (match style with
                                                          | Some style -> Some (fh_style ^ style)
                                                          | None       -> Some fh_style)
                                             | None   -> None)
                                     ?attrs)
             content

  end

end
