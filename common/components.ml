open Containers
open Tyxml

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

  let cons_option = List.cons_maybe

  let map_cons_option ~f opt l =
    Option.map_or ~default:l (fun x -> (f x) :: l) opt

  let cons_if case x l =
    if case then x :: l else l

  let map_cons_if ~f case x l =
    if case then (f x) :: l else l

  let add_common_attrs ?id ?style ?(attrs=[]) l =
    map_cons_option ~f:Html.a_id id l
    |> map_cons_option ~f:Html.a_style style
    |> List.append attrs

  module Box = struct

    let base_class = "mdc-box"
    let vertical_class   = CSS.add_modifier base_class "vertical"
    let horizontal_class = CSS.add_modifier base_class "horizontal"

    let get_justify_content_class x =
      let _class = CSS.add_modifier base_class "justify-content-" in
      _class ^ (match x with
                | `Start         -> "start"
                | `End           -> "end"
                | `Center        -> "center"
                | `Space_between -> "space-between"
                | `Space_around  -> "space-around"
                | `Space_evenly  -> "space-evenly")

    let get_align_items_class x =
      let _class = CSS.add_modifier base_class "align-items-" in
      _class ^ (match x with
                | `Start    -> "start"
                | `End      -> "end"
                | `Center   -> "center"
                | `Stretch  -> "stretch"
                | `Baseline -> "baseline")

    let get_align_content_class x =
      let _class = CSS.add_modifier base_class "align-content-" in
      _class ^ (match x with
                | `Start         -> "start"
                | `End           -> "end"
                | `Center        -> "center"
                | `Stretch       -> "stretch"
                | `Space_between -> "space-between"
                | `Space_around  -> "space-around")

    let create ?id ?style ?(classes=[]) ?attrs ?tag
          ?justify_content ?align_items ?align_content ?(vertical=false) ~content () =
      let tag = Option.get_or ~default:div tag in
      tag ~a:([ a_class (classes
                         |> cons_if vertical vertical_class
                         |> map_cons_option ~f:get_justify_content_class justify_content
                         |> map_cons_option ~f:get_align_items_class align_items
                         |> map_cons_option ~f:get_align_content_class align_content
                         |> List.cons base_class) ]
              |> add_common_attrs ?id ?style ?attrs)
        content

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

  module Icon = struct

    let base_class   = "mdc-icon"
    let button_class = CSS.add_modifier base_class "button"

    module Font = struct

      let create ?id ?style ?(classes=[]) ?attrs ~icon () =
        Html.i ~a:([a_class ("material-icons" :: base_class :: classes)]
                   |> add_common_attrs ?id ?style ?attrs)
               [pcdata icon]

    end

  end

  module Avatar = struct

    let base_class  = "mdc-avatar"
    let dense_class = CSS.add_modifier base_class "dense"

    module Image = struct

      let create ?id ?style ?(classes=[]) ?attrs ?(dense=false) ~src () =
        img ~a:([ a_class (classes
                           |> cons_if dense dense_class
                           |> List.cons base_class) ]
                |> add_common_attrs ?id ?style ?attrs)
          ~src:(uri_of_string src)
          ~alt:""

    end

    module Font_icon = struct

      let icon_class = CSS.add_modifier base_class "icon"

      let create ?id ?style ?(classes=[]) ?attrs ?(dense=false) ~icon () =
        div ~a:([ a_class (classes
                           |> cons_if dense dense_class
                           |> List.cons base_class)]
                |> add_common_attrs ?id ?style ?attrs)
          [icon]

    end

    module Letter = struct

      let letter_class = CSS.add_modifier base_class "letter"

      let create ?id ?style ?(classes=[]) ?attrs ?(dense=false) ~text () =
        div ~a:([ a_class (classes
                           |> cons_if dense dense_class
                           |> List.cons letter_class
                           |> List.cons base_class)]
                |> add_common_attrs ?id ?style ?attrs)
          [pcdata text]

    end

  end

  module Button = struct

    let base_class = "mdc-button"
    let icon_class = CSS.add_element base_class "icon"

    let unelevated_class = CSS.add_modifier base_class "unelevated"
    let stroked_class    = CSS.add_modifier base_class "stroked"
    let raised_class     = CSS.add_modifier base_class "raised"

    let dense_class      = CSS.add_modifier base_class "dense"
    let compact_class    = CSS.add_modifier base_class "compact"

    let create ?(classes=[]) ?id ?style ?button_type ?button_style
          ?(disabled=false) ?(ripple=false)?(dense=false) ?(compact=false)
          ?icon ?label ?onclick ?attrs () =
      button ~a:([ a_class (classes
                            |> map_cons_option ~f:(function
                                   | `Raised     -> raised_class
                                   | `Stroked    -> stroked_class
                                   | `Unelevated -> unelevated_class) button_style
                            |> cons_if dense      dense_class
                            |> cons_if compact    compact_class
                            |> List.cons base_class) ]
                 |> add_common_attrs ?id ?style ?attrs
                 |> map_cons_option ~f:a_onclick onclick
                 |> map_cons_option ~f:a_button_type button_type
                 |> cons_if disabled @@ a_disabled ()
                 |> cons_if ripple   @@ a_user_data "mdc-auto-init" "MDCRipple")
        ((map_cons_option ~f:pcdata label [])
         |> map_cons_option ~f:(fun x -> Html.i ~a:[ a_class [icon_class;"material-icons"] ]
                                           [pcdata x])
              icon)
  end

  module Divider = struct

    let base_class  = "mdc-divider"
    let inset_class = CSS.add_modifier base_class "inset"

    let create ?id ?style ?(classes=[]) ?attrs ?(inset=false) () =
      hr ~a:([ a_class (classes
                        |> cons_if inset inset_class
                        |> List.cons base_class) ]
             |> add_common_attrs ?id ?style ?attrs)
        ()

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

      let _class         = CSS.add_element base_class "actions"
      let action_class   = CSS.add_element base_class "action"
      let vertical_class = CSS.add_modifier _class "vertical"

      let create ?(classes=[]) ?id ?style ?attrs ?(vertical=false) ~children () =
        section ~a:([ a_class (classes
                               |> cons_if vertical vertical_class
                               |> List.cons _class) ]
                    |> add_common_attrs ?id ?style ?attrs)
          children

    end

    module Primary = struct

      let _class            = CSS.add_element base_class "primary"
      let title_class       = CSS.add_element base_class "title"
      let subtitle_class    = CSS.add_element base_class "subtitle"
      let large_title_class = CSS.add_modifier title_class "large"

      let create_title ?(classes=[]) ?id ?style ?attrs ?(large=false) ~title () =
        h1 ~a:([a_class (classes
                         |> cons_if large large_title_class
                         |> List.cons title_class)]
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

    let create ?id ?style ?(classes=[]) ?attrs ?tag ~sections () =
      let tag = Option.get_or ~default:div tag in
      tag ~a:([a_class (base_class :: classes)]
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
                        |> List.cons base_class)]
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
                   []]
            ; div ~a:[a_class [mixedmark_class]] []]]

  end

  module Dialog = struct

    let base_class     = "mdc-dialog"
    let surface_class  = CSS.add_element base_class "surface"
    let backdrop_class = CSS.add_element base_class "backdrop"

    module Header = struct

      let _class = CSS.add_element base_class "header"
      let title_class  = CSS.add_element _class "title"

      let create ?id ?style ?(classes=[]) ?attrs ~title () =
        header ~a:([ a_class [_class]])
          [ h2 ~a:([ a_class (title_class :: classes) ]
                   |> add_common_attrs ?id ?style ?attrs)
              [ pcdata title ]]
    end

    module Body = struct

      let _class           = CSS.add_element base_class "body"
      let scrollable_class = CSS.add_modifier _class "scrollable"

      let create ?(scrollable=false) ?id ?style ?(classes=[]) ?attrs ~content () =
        section ~a:([ a_class (classes
                               |> cons_if scrollable scrollable_class 
                               |> List.cons _class) ]
                    |> add_common_attrs ?id ?style ?attrs)
          content

    end

    module Footer = struct

      let _class = CSS.add_element base_class "footer"
      let button_class = CSS.add_element _class "button"
      let accept_button_class = CSS.add_modifier button_class "accept"
      let cancel_button_class = CSS.add_modifier button_class "cancel"

      let create_button ?id ?style ?(classes=[]) ?attrs ?ripple ?(action=false) ~typ ~label () =
        Button.create ~classes:((match typ with
                                 | `Accept  -> accept_button_class
                                 | `Decline -> cancel_button_class) :: classes
                                |> cons_if action @@ CSS.add_element base_class "action"
                                |> List.cons button_class)
          ?id ?style ?attrs ?ripple ~label ()

      let create ?id ?style ?(classes=[]) ?attrs ~children () =
        footer ~a:([ a_class (_class :: classes)]
                   |> add_common_attrs ?id ?style ?attrs) children

    end

    let create ?id ?style ?(classes=[]) ?attrs ?label_id ?description_id ~content () =
      aside ~a:([ a_class (base_class :: classes)
                ; a_role ["alertdialog"]]
                |> map_cons_option ~f:(fun x -> a_aria "labelledby" [x]) label_id
                |> map_cons_option ~f:(fun x -> a_aria "describedby" [x]) description_id
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
    let mini_class   = CSS.add_modifier base_class "mini"

    let create ?id ?style ?(classes=[]) ?attrs ?onclick
          ?(mini=false) ?(ripple=false) ?label ~icon () =
      button ~a:([ a_class (classes
                            |> cons_if mini @@ mini_class
                            |> List.cons base_class
                            |> List.cons "material-icons") ]
                 |> add_common_attrs ?id ?style ?attrs
                 |> cons_if ripple @@ a_user_data "mdc-auto-init" "MDCRipple"
                 |> map_cons_option ~f:(fun x -> a_aria "label" [x]) label
                 |> map_cons_option ~f:a_onclick onclick)
        [ span ~a:[a_class [icon_class]] [pcdata icon] ]

  end

  module Form_field = struct

    let base_class      = "mdc-form-field"
    let align_end_class = CSS.add_modifier base_class "align-end"

    module Label = struct

      let create ?id ?for_id ?style ?(classes=[]) ?attrs ~label () =
        Html.label ~a:([ a_class classes ]
                       |> map_cons_option ~f:a_label_for for_id
                       |> add_common_attrs ?id ?style ?attrs)
          [pcdata label]

    end

    let create ?id ?style ?(classes=[]) ?attrs ?(align_end=false) ~input ~label () =
      div ~a:([ a_class (classes
                         |> cons_if align_end align_end_class
                         |> List.cons base_class) ]
              |> add_common_attrs ?id ?style ?attrs)
        [ input; label ]

  end

  module Dynamic_grid = struct
    let base_class = "mdc-dynamic-grid"

    module Item = struct

      let _class         = CSS.add_element base_class "item"
      let ghost_class    = CSS.add_element _class "ghost"
      let resize_class   = CSS.add_element _class "resize"
      let dragging_class = CSS.add_modifier _class "dragging"

      let create_ghost ?id ?style ?(classes=[]) ?attrs () =
        div ~a:([ a_class (ghost_class :: classes)]
                |> add_common_attrs ?id ?style ?attrs)
          []

      let create_resize_button ?id ?style ?(classes=[]) ?attrs () =
        span ~a:([ a_class (resize_class :: classes)]
                 |> add_common_attrs ?id ?style ?attrs)
          []

      let create ?id ?style ?(classes=[]) ?attrs ?resize_button () =
        div ~a:([ a_class (_class :: classes)
                  (* ; a_draggable true *) ]
                |> add_common_attrs ?id ?style ?attrs)
          (cons_option resize_button [])
    end

    let create ?id ?style ?(classes=[]) ?attrs ~items () =
      div ~a:([ a_class (base_class :: classes) ]
              |> add_common_attrs ?id ?style ?attrs)
        items
  end

  module Grid_list = struct

    let base_class             = "mdc-grid-list"
    let tiles_class            = CSS.add_element base_class "tiles"
    let tile_gutter_1_class    = CSS.add_modifier base_class "tile-gutter-1"
    let icon_align_start_class = CSS.add_modifier base_class "with-icon-align-start"
    let icon_align_end_class   = CSS.add_modifier base_class "with-icon-align-end"
    let header_caption_class   = CSS.add_modifier base_class "header-caption"
    let twoline_caption_class  = CSS.add_modifier base_class "twoline-caption"

    module Tile = struct

      let _class = "mdc-grid-tile"

      module Primary = struct

        let primary_class = CSS.add_element _class "primary"
        let content_class = CSS.add_element _class "primary-content"

        let create_content ?id ?style ?(classes=[]) ?attrs ?src ?alt ?(is_div=false) () =
          if not is_div
          then img ~src:(Html.uri_of_string (Option.get_or ~default:"" src))
                 ~alt:(Option.get_or ~default:"" alt)
                 ~a:([ a_class (content_class :: classes) ]
                     |> add_common_attrs ?id ?style ?attrs) ()
          else let style = (match src with
                            | Some x -> Printf.sprintf "background-image: url(%s);" x
                                        |> (fun s -> Some (match style with
                                                           | Some x -> s ^ x
                                                           | None   -> s))
                            | None -> style) in
               div ~a:([ a_class (content_class :: classes) ]
                       |> add_common_attrs ?id ?style ?attrs)
                 []

        let create ?id ?style ?(classes=[]) ?attrs ~content () =
          div ~a:([ a_class (primary_class :: classes) ]
                  |> add_common_attrs ?id ?style ?attrs)
            [ content ]

      end

      module Caption = struct

        let secondary_class    = CSS.add_element _class "secondary"
        let icon_class         = CSS.add_element _class "icon"
        let title_class        = CSS.add_element _class "title"
        let support_text_class = CSS.add_element _class "support-text"

        let create_title ?id ?style ?(classes=[]) ?attrs ~text () =
          span ~a:([ a_class (title_class :: classes) ]
                   |> add_common_attrs ?id ?style ?attrs)
            [pcdata text]

        let create_support_text ?id ?style ?(classes=[]) ?attrs ~text () =
          span ~a:([ a_class (support_text_class :: classes) ]
                   |> add_common_attrs ?id ?style ?attrs)
            [pcdata text]

        let create ?id ?style ?(classes=[]) ?attrs ?title ?support_text ?icon () =
          span ~a:([ a_class (secondary_class :: classes) ]
                   |> add_common_attrs ?id ?style ?attrs)
            (cons_option support_text []
             |> cons_option title
             |> cons_option icon)

      end

      let create ?id ?style ?(classes=[]) ?attrs ?caption ~primary () =
        li ~a:([ a_class (_class :: classes)]
               |> add_common_attrs ?id ?style ?attrs)
          (cons_option caption [] |> List.cons primary)

    end

    let ar_to_string = function
      | `AR_1_1  -> "1x1" | `AR_16_9 -> "16x9" | `AR_2_3  -> "2x3"
      | `AR_3_2  -> "3x2" | `AR_4_3  -> "4x3"  | `AR_3_4  -> "3x4"

    let ar_to_class x = CSS.add_modifier base_class ("tile-aspect-" ^ (ar_to_string x))

    let create ?id ?style ?(classes=[]) ?attrs
          ?ar ?(one_px_gutter=false) ?(header_caption=false) ?(twoline=false) ?icon_align
          ~tiles () =
      div ~a:([ a_class (base_class :: classes
                         |> map_cons_option ~f:ar_to_class ar
                         |> cons_if one_px_gutter  tile_gutter_1_class
                         |> cons_if header_caption header_caption_class
                         |> cons_if twoline        twoline_caption_class
                         |> map_cons_option ~f:(function
                                | `Start -> icon_align_start_class
                                | `End   -> icon_align_end_class)
                              icon_align) ]
              |> add_common_attrs ?id ?style ?attrs)
        [ ul ~a:[ a_class [tiles_class] ] tiles ]

  end

  module Icon_toggle = struct

    type data =
      { icon      : string        [@key "content"]
      ; label     : string option
      ; css_class : string option [@key "cssClass"]
      } [@@deriving to_yojson]

    let base_class  = "mdc-icon-toggle"
    let icons_class = "material-icons"

    let create ?id ?style ?(classes=[]) ?attrs ?(disabled=false) ?color_scheme ~on_data ~off_data () =
      let data_toggle_on  = on_data |> data_to_yojson |> Yojson.Safe.to_string in
      let data_toggle_off = off_data |> data_to_yojson |> Yojson.Safe.to_string in
      i ~a:([ a_class (classes
                       |> cons_if disabled @@ CSS.add_modifier base_class "disabled"
                       |> map_cons_option ~f:(function
                              | `Primary -> CSS.add_modifier base_class "primary"
                              | `Accent  -> CSS.add_modifier base_class "accent")
                            color_scheme
                       |> List.cons base_class
                       |> List.cons icons_class)
            ; a_role ["button"]
            ; a_user_data "toggle-on"  data_toggle_on
            ; a_user_data "toggle-off" data_toggle_off
            ; a_aria "pressed" ["false"]
            ; a_tabindex (if disabled then -1 else 0) ]
            |> map_cons_option ~f:(fun x -> a_aria "label" [x]) off_data.label
            |> cons_if disabled @@ a_aria "disabled" ["true"]
            |> add_common_attrs ?id ?style ?attrs)
        [pcdata off_data.icon]

  end

  module Layout_grid = struct

    let max_columns = 12

    let check_columns_number_exn n =
      if n > max_columns || n < 0 then failwith "Layout grid: bad columns number"

    let base_class               = "mdc-layout-grid"
    let inner_class              = CSS.add_element base_class "inner"
    let fixed_column_width_class = CSS.add_modifier base_class "fixed-column-width"

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
        CSS.add_modifier _class ("order-" ^ (string_of_int n))

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
                         |> cons_if fixed_column_width fixed_column_width_class
                         |> List.cons base_class) ]
              |> add_common_attrs ?id ?style ?attrs)
        content

  end

  module Circular_progress = struct


    let sz                  = 50.
    let base_class          = "mdc-circular-progress"
    let circle_class        = CSS.add_element  base_class "circle"
    let indeterminate_class = CSS.add_modifier base_class "indeterminate"
    let primary_class       = CSS.add_modifier base_class "primary"
    let secondary_class     = CSS.add_modifier base_class "secondary"

    let create ?id ?style ?(classes=[]) ?attrs ?(indeterminate=true) ?(thickness=3.6) ?(size=40) () =
      let style = Printf.sprintf "width: %dpx; height: %dpx" size size
                  |> (fun x -> match style with
                               | Some s -> x ^ "; " ^ s
                               | None   -> x)
      in
      div ~a:([ a_class (classes
                         |> cons_if indeterminate indeterminate_class
                         |> List.cons base_class)
              ; a_role ["progressbar"]]
              |> add_common_attrs ?id ~style ?attrs)
        [ svg ~a:([ Svg.a_class []
                  ; Svg.a_viewBox (0.,0.,sz,sz)])
            [ Svg.circle ~a:[ Svg.a_class [circle_class]
                            ; Svg.a_cx (sz /. 2., None)
                            ; Svg.a_cy (sz /. 2., None)
                            ; Svg.a_fill `None
                            ; Svg.a_stroke_width (thickness, None)
                            ; Svg.a_r ((sz /. 2.) -. 5., None)] []]]

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
                         |> List.cons base_class) ]
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

    module Item = struct

      let _class               = "mdc-list-item"
      let text_class           = CSS.add_element _class "text"
      let secondary_text_class = CSS.add_element _class "secondary-text"
      let start_detail_class   = CSS.add_element _class "start-detail"
      let end_detail_class     = CSS.add_element _class "end-detail"
      let divider_class        = "mdc-list-divider"

      let create_divider ?id ?style ?(classes=[]) ?attrs ?(tag=div) ?(inset=false) () =
        tag ~a:([ a_class (classes
                           |> cons_if inset @@ CSS.add_modifier divider_class "inset"
                           |> List.cons divider_class)
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
        tag ~a:(
            [ a_class (subheader_class :: classes) ]
            |> add_common_attrs ?id ?style ?attrs)
          [pcdata text]

      let create ?id ?style ?(classes=[]) ?attrs ~content () =
        div ~a:([ a_class (_class :: classes)]
                |> add_common_attrs ?id ?style ?attrs)
          content

    end

    let dense_class    = CSS.add_modifier base_class "dense"
    let two_line_class = CSS.add_modifier base_class "two-line"
    let avatar_class   = CSS.add_modifier base_class "avatar-list"
    let bordered_class = CSS.add_modifier base_class "bordered"

    let create ?id ?style ?(classes=[]) ?attrs ?(tag=div) ?(avatar=false)
          ?(dense=false) ?(bordered=false) ?(two_line=false) ~items () =
      tag ~a:([ a_class (classes
                         |> cons_if bordered bordered_class
                         |> cons_if dense    dense_class
                         |> cons_if two_line two_line_class
                         |> cons_if avatar   avatar_class
                         |> List.cons base_class) ]
              |> add_common_attrs ?id ?style ?attrs)
        items

  end

  module Tree = struct

    let base_class = "mdc-tree"

    module Item = struct

      let _class                   = CSS.add_element base_class "item"
      let nested_list_class        = CSS.add_element _class "nested-list"
      let nested_list_hidden_class = CSS.add_modifier nested_list_class "hidden"

      let create_item = List_.Item.create

      let create_nested_list = List_.create ~classes:[nested_list_class]

      let create_divider = List_.Item.create_divider

      let create ?id ?style ?(classes=[]) ?attrs ?nested_list ~item () =
        Html.div ~a:([ a_class (_class :: classes) ]
                     |> add_common_attrs ?id ?style ?attrs)
          (cons_option nested_list []
           |> List.cons item)

    end

    let create = List_.create ~classes:[base_class]

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
                              |> List.cons _class)
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
                           |> List.cons Typography.base_class
                           |> List.cons base_class) ]
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
                             |> List.cons Typography.base_class
                             |> List.cons base_class) ]
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
                             |> List.cons Typography.base_class
                             |> List.cons base_class) ]
                  |> add_common_attrs ?id ?style ?attrs)
          [ nav ~a:([ a_class [drawer_class] ])
              content
          ]

    end

  end

  module Expansion_panel = struct

    let base_class          = "mdc-expansion-panel"
    let expanded_class      = CSS.add_modifier base_class "expanded"
    let panel_wrapper_class = CSS.add_element base_class "panel-wrapper"

    module Primary = struct

      let _class        = CSS.add_element base_class "primary"
      let summary_class = CSS.add_element base_class "summary"
      let details_class = CSS.add_element base_class "details"
      let heading_class = CSS.add_element base_class "heading"
      let icon_class    = CSS.add_element base_class "icon"

      let create ?id ?style ?(classes=[]) ?attrs ?(heading_details=[]) ?(details=[]) ~title () =
        div ~a:([ a_class (classes |> CCList.cons _class)
                ; a_tabindex 0 ]
                |> add_common_attrs ?id ?style ?attrs)
            [ div ~a:([ a_class [summary_class] ])
                  [ div ~a:([ a_class [heading_class]])
                        [ pcdata title
                        ; div ~a:[ a_class [details_class]] heading_details
                        ]
                  ; div ~a:([ a_class [details_class]]) details
                  ]
            ; div ~a:([ a_class [icon_class]; a_tabindex (-1) ])
                  [Icon.Font.create ~icon:"expand_more" ()]
            ]

    end

    module Actions = struct

      let _class       = CSS.add_element base_class "actions"
      let action_class = CSS.add_element base_class "action"

      let create ?id ?style ?(classes=[]) ?attrs ~actions () =
        div ~a:([ a_class (classes |> CCList.cons _class) ]
                |> add_common_attrs ?id ?style ?attrs)
            actions

    end

    module Panel = struct

      let _class = CSS.add_element base_class "panel"

      let create ?id ?style ?(classes=[]) ?attrs ~content () =
        div ~a:([ a_class (classes |> CCList.cons _class) ]
                |> add_common_attrs ?id ?style ?attrs)
            content

    end

    let create ?id ?style ?(classes=[]) ?attrs ?actions ~primary ~panel () =
      div ~a:([ a_class (classes |> CCList.cons base_class)]
              |> add_common_attrs ?id ?style ?attrs)
          [ primary
          ; div ~a:[a_class [panel_wrapper_class]]
                (match actions with
                 | Some x -> panel :: Divider.create () :: x :: []
                 | None   -> [panel])]

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
                  |> (fun x -> x @ (Option.get_or ~default:[] attrs)))
          ~text ?text_id ?text_style ?text_classes ?text_attrs ?secondary_text
          ?secondary_text_id ?secondary_text_style ?secondary_text_classes ?secondary_text_attrs
          ?start_detail ?end_detail ()

    end

    let create_list ?id ?style ?classes ?attrs ~items () =
      List_.create ?id ?style ~classes:([items_class]
                                        |> (fun x -> x @ (Option.get_or ~default:[] classes)))
        ~attrs:([ a_role ["menu"]
                ; a_aria "hidden" ["true"] ]
                |> (fun x -> x @ (Option.get_or ~default:[] attrs)))
        ~items
        ()

    let create ?id ?style ?(classes=[]) ?attrs ?(opened=false) ?open_from ~list () =
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
                         |> List.cons base_class)
              ; a_tabindex (-1) ]
              |> add_common_attrs ?id ?style ?attrs)
        [ list ]

  end

  module Radio = struct

    let base_class           = "mdc-radio"
    let native_control_class = CSS.add_element base_class "native-control"
    let background_class     = CSS.add_element base_class "background"
    let outer_circle_class   = CSS.add_element base_class "outer-circle"
    let inner_circle_class   = CSS.add_element base_class "inner-circle"
    let disabled_class       = CSS.add_modifier base_class "disabled"

    let create ?id ?input_id ?style ?(classes=[]) ?attrs ?(auto_init=true)
          ?(checked=false) ?(disabled=false) ?name () =
      div ~a:([ a_class (classes
                         |> cons_if disabled disabled_class
                         |> List.cons base_class) ]
              |> cons_if auto_init @@ a_user_data "mdc-auto-init" "MDCRadio"
              |> add_common_attrs ?id ?style ?attrs)
        [ input ~a:([ a_class [native_control_class]
                    ; a_input_type `Radio ]
                    |> map_cons_option ~f:a_name name
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

  module Select = struct

    let base_class          = "mdc-select"
    let surface_class     = CSS.add_element base_class "surface"
    let bottom_line_class = CSS.add_element base_class "bottom-line"

    module Base = struct

      let menu_class              = CSS.add_element base_class "menu"
      let label_class             = CSS.add_element base_class "label"
      let label_float_above_class = CSS.add_modifier label_class "float-above"
      let selected_text_class     = CSS.add_element base_class "selected-text"
      let disabled_class          = CSS.add_modifier base_class "disabled"

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
                    |> (fun x -> x @ (Option.get_or ~default:[] attrs)))
            ~text ?text_id ?text_style ?text_classes ?text_attrs ?start_detail ?end_detail ()

      end

      let create_list ?id ?style ?attrs ?(classes=[]) ~items () =
        List_.create ?id ?style ?attrs ~classes:(Menu.items_class :: classes) ~items ()

      let create_menu ?id ?style ?(classes=[]) ?attrs ~list () =
        Menu.create ?id ?style ?attrs ~classes:(menu_class :: classes) ~list ()

      let create ?id ?style ?(classes=[]) ?attrs ?(disabled=false) ~label ~menu () =
        div ~a:([ a_class (classes
                           |> cons_if disabled disabled_class
                           |> List.cons base_class)
                ; a_role ["listbox"]]
                |> cons_if (not disabled) @@ a_tabindex 0
                |> cons_if disabled       @@ a_tabindex (-1)
                |> cons_if disabled       @@ a_aria "disabled" ["true"]
                |> add_common_attrs ?id ?style ?attrs)
          [ div ~a:[ a_class [surface_class]]
              [ div ~a:[ a_class [label_class]] [pcdata label]
              ; div ~a:[ a_class [selected_text_class]] []
              ; div ~a:[ a_class [bottom_line_class]] [] ]
          ; menu
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
        div ~a:([ a_class (base_class :: classes)]
                |> add_common_attrs ?id ?style ?attrs)
          [ select ~a:([ a_class [surface_class] ]
                       |> cons_if disabled @@ a_disabled ())
              items
          ; div ~a:([ a_class [bottom_line_class]]) [] ]

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
               ?label ?step ?(min=0.) ?(max=100.) ?(value=0.) () =
      let discrete = if markers then true else discrete in
      div ~a:([ a_class (classes
                         |> cons_if discrete @@ CSS.add_modifier base_class "discrete"
                         |> cons_if (discrete && markers) @@ CSS.add_modifier base_class "display-markers"
                         |> List.cons base_class)
              ; a_tabindex 0
              ; a_role ["slider"]
              ; a_aria "valuemin" [ string_of_float min ]
              ; a_aria "valuemax" [ string_of_float max ]
              ; a_aria "valuenow" [ string_of_float value ] ]
              |> map_cons_option ~f:(fun x -> a_aria "label" [x]) label
              |> map_cons_option ~f:(fun x -> a_user_data "step" (string_of_float x)) step
              |> cons_if disabled @@ a_aria "disabled" ["true"]
              |> add_common_attrs ?id ?style ?attrs)
        [ div ~a:([ a_class [track_container_class]])
            (cons_if (discrete && markers) (div ~a:[ a_class [track_marker_container_class]] []) []
             |> List.cons @@ div ~a:([ a_class [track_class]]) [])
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
    let align_start_class    = CSS.add_modifier base_class "align-start"

    let create ?id ?style ?(classes=[]) ?attrs ?(start_aligned=false) () =
      div ~a:([ a_class (classes
                         |> cons_if start_aligned align_start_class
                         |> List.cons base_class)
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
    let disabled_class       = CSS.add_modifier base_class "disabled"

    let create ?id ?input_id ?style ?(classes=[]) ?attrs ?(disabled=false) () =
      div ~a:([ a_class (classes
                         |> cons_if disabled disabled_class
                         |> List.cons base_class) ]
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
      let active_class    = CSS.add_modifier _class "active"

      let create ?id ?style ?(classes=[]) ?attrs ?(active=false) ?href ~content () =
        a ~a:([ a_class (classes
                         |> cons_if active active_class
                         |> List.cons _class) ]
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

      let _class                 = "mdc-tab-bar"
      let indicator_primary_class = CSS.add_modifier _class "indicator-primary"
      let indicator_accent_class  = CSS.add_modifier _class "indicator-accent"

      module Indicator = struct

        let _class = CSS.add_element _class "indicator"

        let create ?id ?style ?(classes=[]) ?attrs () =
          span ~a:([a_class (_class :: classes)]
                   |> add_common_attrs ?id ?style ?attrs) []

      end

      let create ?id ?style ?(classes=[]) ?attrs
            ?(indicator=Indicator.create ()) ?color_scheme ~typ ~tabs () =
        nav ~a:([ a_class (classes
                           |> (fun x -> match typ with
                                        | `Text          -> x
                                        | `Icon          -> (CSS.add_modifier _class "icon-tab-bar") :: x
                                        | `Text_and_icon -> (CSS.add_modifier _class "icons-with-text") :: x)
                           |> map_cons_option ~f:(function
                                  | `Primary -> indicator_primary_class
                                  | `Accent  -> indicator_accent_class)
                                color_scheme
                           |> List.cons _class) ]
                |> add_common_attrs ?id ?style ?attrs)
          (tabs @ [indicator])

    end

    module Scroller = struct

      let _class                  = "mdc-tab-bar-scroll"
      let container_class         = CSS.add_element _class "container"
      let tab_bar_wrapper_class   = CSS.add_element _class "tab-bar-wrapper"

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

      let create ?id ?style ?(classes=[]) ?attrs ~tabs () =
        div ~a:([ a_class (_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs)
            [ div ~a:[ a_class [container_class]]
                  [ create_indicator ~direction:`Back ()
                  ; div ~a:[ a_class [tab_bar_wrapper_class]
                           ; a_role ["tablist"]] [ tabs ]
                  ; create_indicator ~direction:`Forward ()
                  ]
            ]

    end

  end

  module Textfield = struct

    let base_class        = "mdc-text-field"
    let input_class       = CSS.add_element base_class "input"
    let label_class       = CSS.add_element base_class "label"
    let bottom_line_class = CSS.add_element base_class "bottom-line"

    module Wrapper = struct

      let _class = "mdc-text-field-wrapper"

      let create ?id ?style ?(classes=[]) ?attrs ~textfield ~helptext () =
        section ~a:([ a_class (classes
                               |> List.cons _class) ]
                    |> add_common_attrs ?id ?style ?attrs)
          [ textfield; helptext ]

    end

    module Help_text = struct

      let _class               = "mdc-text-field-helper-text"
      let persistent_class     = CSS.add_modifier _class "persistent"
      let validation_msg_class = CSS.add_modifier _class "validation-msg"

      let create ?id ?style ?(classes=[]) ?attrs ?(persistent=false) ?(validation=false) ?text () =
        p ~a:([ a_class (classes
                         |> cons_if validation validation_msg_class
                         |> cons_if persistent persistent_class
                         |> List.cons _class) ]
              |> cons_if (not persistent) @@ a_aria "hidden" ["true"]
              |> add_common_attrs ?id ?style ?attrs)
          [pcdata @@ Option.get_or ~default:"" text]

    end

    module Icon = struct

      let _class       = CSS.add_element base_class "icon"

      let create ?id ?style ?(classes=[]) ?attrs ?(clickable=true) ~icon () =
        Html.i ~a:([ a_class ("material-icons" :: _class :: classes) ]
                   |> cons_if clickable @@ a_tabindex 0
                   |> add_common_attrs ?id ?style ?attrs)
          [pcdata icon]

    end

    let textarea_class          = CSS.add_modifier base_class "textarea"
    let dense_class             = CSS.add_modifier base_class "dense"
    let fullwidth_class         = CSS.add_modifier base_class "fullwidth"
    let disabled_class          = CSS.add_modifier base_class "disabled"
    let box_class               = CSS.add_modifier base_class "box"
    let upgraded_class          = CSS.add_modifier base_class "upgraded"
    let label_float_above_class = CSS.add_modifier label_class "float-above"

    let create ?id ?style ?(classes=[]) ?attrs ?placeholder
          ?label_id ?label_style ?(label_classes=[]) ?label_attrs
          ?value ?(input_type=`Text) ?(disabled=false) ?label ?input_id ?help_text_id
          ?leading_icon ?trailing_icon ?(box=false) ?(required=false) ?(full_width=false) ?(dense=false)
          ?(textarea=false) ?rows ?cols () =
      div ~a:([ a_class (classes
                         |> cons_if textarea   textarea_class
                         |> cons_if full_width fullwidth_class
                         |> cons_if dense      dense_class
                         |> cons_if disabled   disabled_class
                         |> cons_if box        box_class
                         |> cons_if (Option.is_some leading_icon) (CSS.add_modifier base_class "with-leading-icon")
                         |> cons_if (Option.is_some trailing_icon) (CSS.add_modifier base_class "with-trailing-icon")
                         |> cons_if (Option.is_some value) upgraded_class
                         |> List.cons base_class) ]
              |> add_common_attrs ?id ?style ?attrs)
        ((if textarea then [] else [ div ~a:([ a_class [bottom_line_class]]) [] ])
         |> cons_option trailing_icon
         |> map_cons_option ~f:(fun x ->
                (Html.label ~a:([ a_class (label_classes
                                           |> cons_if (Option.is_some value) label_float_above_class
                                           |> List.cons label_class) ]
                                |> map_cons_option ~f:a_label_for input_id
                                |> add_common_attrs ?id:label_id
                                     ?style:label_style
                                     ?attrs:label_attrs)
                   [pcdata x]))
              label
         |> List.cons (let common_attrs = ([ a_class [input_class] ]
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
                                (pcdata @@ Option.get_or ~default:"" value))))
         |> cons_option leading_icon)

  end

  module Toolbar = struct

    let base_class                = "mdc-toolbar"
    let fixed_adjust_class        = base_class ^ "-fixed-adjust"
    let fixed_class               = CSS.add_modifier base_class "fixed"
    let waterfall_class           = CSS.add_modifier base_class "waterfall"
    let fixed_last_row_only_class = CSS.add_modifier base_class "fixed-lastrow-only"
    let flexible_class            = CSS.add_modifier base_class "flexible"

    module Row = struct

      module Section = struct

        let _class = CSS.add_element base_class "section"
        let title_class         = CSS.add_element base_class "title"
        let icon_class          = CSS.add_element base_class "icon"
        let menu_icon_class     = CSS.add_element base_class "menu-icon"
        let align_start_class   = CSS.add_modifier _class "align-start"
        let align_end_class     = CSS.add_modifier _class "align-end"
        let shrink_to_fit_class = CSS.add_modifier _class "shrink-to-fit"

        let create_title ?id ?style ?(classes=[]) ?attrs ~title () =
          span ~a:([ a_class (title_class :: classes)]
                   |> add_common_attrs ?id ?style ?attrs)
            [ pcdata title ]

        let create ?id ?style ?(classes=[]) ?attrs ?align ?(shrink_to_fit=false) ~content () =
          section ~a:([ a_class (classes
                                 |> map_cons_option ~f:(function
                                        | `Start -> align_start_class
                                        | `End   -> align_end_class) align
                                 |> cons_if shrink_to_fit shrink_to_fit_class
                                 |> List.cons _class) ]
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
                            |> cons_if fixed fixed_class
                            |> cons_if (fixed && waterfall) waterfall_class
                            |> cons_if (fixed && fixed_last_row) fixed_last_row_only_class
                            |> cons_if flexible flexible_class
                            |> List.cons base_class) ]
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

  module Table = struct

    let base_class      = "mdc-data-table"
    let container_class = CSS.add_element base_class "container"

    module Cell = struct

      type 'a content = Text  of string
                      | Float of float
                      | Int   of int
                      | Int32 of int32
                      | Int64 of int64
                      | Other of 'a elt

      let _class         = CSS.add_element  base_class "cell"
      let numeric_class  = CSS.add_modifier base_class "numeric"
      let dense_class    = CSS.add_modifier base_class "dense"
      let checkbox_class = CSS.add_modifier base_class "checkbox"

      let create ?id ?style ?(classes=[]) ?attrs ?(header=false) ?(dense=false) ?(checkbox=false) ~content () =
        let tag = if header then th else td in
        tag ~a:([ a_class (classes
                           |> (fun l -> match content with
                                        | Text _ -> l
                                        | Float _ | Int _ | Int32 _ | Int64 _ -> numeric_class :: l
                                        | _      -> l)
                           |> cons_if dense dense_class
                           |> cons_if checkbox checkbox_class
                           |> List.cons _class)]
                |> add_common_attrs ?id ?style ?attrs)
          (match content with
           | Text s  -> [ pcdata s]
           | Float x -> [ pcdata (string_of_float x) ]
           | Int x   -> [ pcdata (string_of_int x) ]
           | Int32 x -> [ pcdata (Int32.to_string x) ]
           | Int64 x -> [ pcdata (Int64.to_string x) ]
           | Other x -> [ x ])

    end

    module Row = struct

      let _class = CSS.add_element base_class "row"

      let create ?id ?style ?(classes=[]) ?attrs ~cells () =
        tr ~a:([ a_class (classes
                          |> List.cons _class )]
               |> add_common_attrs ?id ?style ?attrs)
          cells

    end

    module Header = struct

      let _class = CSS.add_element base_class "header"

      let create ?id ?style ?(classes=[]) ?attrs ~row () =
        thead ~a:([ a_class (classes
                             |> List.cons _class )]
                  |> add_common_attrs ?id ?style ?attrs)
          [ row ]

    end

    module Body = struct

      let _class = CSS.add_element base_class "body"

      let create ?id ?style ?(classes=[]) ?attrs ~rows () =
        tbody ~a:([ a_class (classes
                             |> List.cons _class ) ]
                  |> add_common_attrs ?id ?style ?attrs)
          rows

    end

    module Footer = struct

      let _class = CSS.add_element base_class "footer"

      let create ?id ?style ?(classes=[]) ?attrs ~row () =
        tfoot ~a:([ a_class (classes
                             |> List.cons _class ) ]
                  |> add_common_attrs ?id ?style ?attrs)
          [ row ]

    end

    let create_table ?id ?style ?(classes=[]) ?attrs ?header ?footer ~body () =
      table ?thead:header
        ?tfoot:footer
        ~a:([ a_class (classes
                       |> List.cons container_class)]
            |> add_common_attrs ?id ?style ?attrs)
        [body]

    let create ?id ?style ?(classes=[]) ?attrs ~table () =
      div ~a:([ a_class (classes
                         |> List.cons base_class)]
              |> add_common_attrs ?id ?style ?attrs)
        [table]

  end

end
