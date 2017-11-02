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

    let create ?(classes=[]) ?style ?id ?input_id ?(disabled=false) ?(js=false) ?(checked=false) ?attrs () =
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

    let create ?id ?style ?(classes=[]) ?attrs ?onclick
               ?(mini=false) ?(plain=false) ?(ripple=false) ?label ~icon () =
      button ~a:([a_class (base_class :: "material-icons" :: classes
                           |> (fun x -> if mini  then x @ [CSS.add_modifier base_class "mini"]  else x)
                           |> (fun x -> if plain then x @ [CSS.add_modifier base_class "plain"] else x))]
                 |> add_common_attrs ?id ?style ?attrs
                 |> (fun x -> if ripple then (a_user_data "mdc-auto-init" "MDCRipple") :: x else x)
                 |> (fun x -> match label with
                              | Some label -> (a_aria "label" [label]) :: x
                              | None       -> x)
                 |> (fun l -> match onclick with
                              | Some x -> l @ [a_onclick x]
                              | None   -> l))
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
             ((match support_text with
               | Some x -> [span ~a:([ a_class (support_text_class :: support_text_classes) ]
                                     |> add_common_attrs ?id:support_text_id
                                                         ?style:support_text_style
                                                         ?attrs:support_text_attrs)
                                 [pcdata x]]
               | None   -> [])
              |> (fun l -> match title with
                           | Some x -> (span ~a:([ a_class (title_class :: title_classes) ]
                                                 |> add_common_attrs ?id:title_id
                                                                     ?style:title_style
                                                                     ?attrs:title_attrs)
                                             [pcdata x]) :: l
                           | None   -> l)
              |> (fun l -> match icon with
                           | Some x -> x :: l
                           | None   -> l))

      let create ?id ?style ?(classes=[]) ?attrs ?primary ?secondary () =
        li ~a:([ a_class (_class :: classes)]
               |> add_common_attrs ?id ?style ?attrs)
           ((match secondary with
             | Some x -> [x]
             | None   -> [])
            |> (fun l -> match primary with
                         | Some x -> x :: l
                         | None   -> l))

    end

    let ar_to_string = function
      | `AR_1_1  -> "1x1" | `AR_16_9 -> "16x9" | `AR_2_3  -> "2x3"
      | `AR_3_2  -> "3x2" | `AR_4_3  -> "4x3"  | `AR_3_4  -> "3x4"

    let create ?id ?style ?(classes=[]) ?attrs
               ?ar ?(one_px_gutter=false) ?(header_caption=false) ?(twoline=false) ?icon_align
               ~tiles () =
      div ~a:([ a_class (base_class :: classes
                         |> (fun x -> match ar with
                                      | Some ar -> let s = ar_to_string ar in
                                                   x @ [CSS.add_modifier base_class ("tile-aspect-" ^ s)]
                                      | None    -> x)
                         |> (fun x -> if one_px_gutter
                                      then x @ [CSS.add_modifier base_class "tile-gutter-1"]
                                      else x)
                         |> (fun x -> if header_caption
                                      then x @ [CSS.add_modifier base_class "header-caption"]
                                      else x)
                         |> (fun x -> if twoline
                                      then x @ [CSS.add_modifier base_class "twoline-caption"]
                                      else x)
                         |> (fun x -> match icon_align with
                                      | Some ia ->
                                         x @ [(match ia with
                                               | `Start -> CSS.add_modifier base_class "with-icon-align-start"
                                               | `End   -> CSS.add_modifier base_class "with-icon-align-end")]
                                      | None    -> x)) ]
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
      i ~a:([ a_class (base_class :: icons_class :: classes
                       |> (fun x -> if disabled then x @ [CSS.add_modifier base_class "disabled"] else x)
                       |> (fun x -> match color_scheme with
                                    | Some scheme -> x @ [(match scheme with
                                                           | `Primary -> "primary"
                                                           | `Accent  -> "accent")
                                                          |> fun s -> CSS.add_modifier base_class s]
                                    | None        -> x))
            ; a_role ["button"]
            ; a_user_data "toggle-on"  data_toggle_on
            ; a_user_data "toggle-off" data_toggle_off
            ; a_aria "pressed" ["false"]
            ; a_tabindex (if disabled then -1 else 0) ]
            |> (fun x -> match off_label with
                         | Some l -> x @ [a_aria "label" [l]]
                         | None   -> x)
            |> (fun x -> if disabled then x @ [ a_aria "disabled" ["true"]] else x)
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
                           |> (fun x -> match span with
                                        | Some span -> x @ [get_cell_span ?device_type:span.device_type
                                                                          span.columns]
                                        | None      -> x)
                           |> (fun x -> match align with
                                        | Some align -> x @ [get_cell_align align]
                                        | None       -> x)
                           |> (fun x -> match order with
                                        | Some order -> x @ [get_cell_order order]
                                        | None       -> x)) ]
                |> add_common_attrs ?id ?style ?attrs)
            content

    end

    let create_inner ?id ?style ?(classes=[]) ?attrs ~cells () =
      div ~a:([ a_class (inner_class :: classes)]
              |> add_common_attrs ?id ?style ?attrs)
          cells

    let create ?id ?style ?(classes=[]) ?attrs ?align ?(fixed_column_width=false) ~content () =
      div ~a:([ a_class (base_class :: classes
                         |> (fun x -> match align with
                                      | Some align -> x @ [get_grid_align align]
                                      | None       -> x)
                         |> (fun x -> if fixed_column_width
                                      then x @ [CSS.add_modifier base_class "fixed-column-width"]
                                      else x)) ]
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
              ; a_class (base_class :: classes
                         |> (fun x -> if indeterminate
                                      then x @ [CSS.add_modifier base_class "indeterminate"]
                                      else x)
                         |> (fun x -> if reversed then x @ [CSS.add_modifier base_class "reversed"] else x)
                         |> (fun x -> if accent then x @ [CSS.add_modifier base_class "accent"] else x)) ]
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

      let create_divider ?id ?style ?(classes=[]) ?attrs ?(tag=li) ?(inset=false) () =
        tag ~a:([ a_class (divider_class :: classes
                           |> (fun x -> if inset then x @ [CSS.add_modifier divider_class "inset"] else x))
                ; a_role ["separator"] ]
                |> add_common_attrs ?id ?style ?attrs)
            []

      let create ?id ?style ?(classes=[]) ?attrs ?(ripple=false) ?(tag=li)
                 ~text ?text_id ?text_style ?(text_classes=[]) ?text_attrs ?secondary_text
                 ?secondary_text_id ?secondary_text_style ?(secondary_text_classes=[]) ?secondary_text_attrs
                 ?start_detail ?end_detail () =
        tag ~a:([ a_class (_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs
                |> (fun x -> if ripple then x @ [a_user_data "mdc-auto-init" "MDCRipple"] else x))
            ((match secondary_text with
              | Some x -> [ span ~a:([ a_class (text_class :: text_classes) ]
                                     |> add_common_attrs ?id:text_id ?style:text_style ?attrs:text_attrs)
                                 [ pcdata text
                                 ; span ~a:([ a_class (secondary_text_class :: secondary_text_classes) ]
                                            |> add_common_attrs ?id:secondary_text_id
                                                                ?style:secondary_text_style
                                                                ?attrs:secondary_text_attrs)
                                        [pcdata x]] ]
              | None   -> [pcdata text])
             |> (fun x -> match start_detail with
                          | Some detail -> detail :: x
                          | None        -> x)
             |> (fun x -> match end_detail with
                          | Some detail -> x @ [detail]
                          | None        -> x))

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

    let create ?id ?style ?(classes=[]) ?attrs ?(tag=ul) ?(avatar=false)
               ?(dense=false) ?(two_line=false) ~items () =
      tag ~a:([ a_class (base_class :: classes
                         |> (fun x -> if dense then x @ [CSS.add_modifier base_class "dense"] else x)
                         |> (fun x -> if two_line then x @ [CSS.add_modifier base_class "two-line"] else x)
                         |> (fun x -> if avatar then x @ [CSS.add_modifier base_class "avatar-list"] else x)) ]
              |> add_common_attrs ?id ?style ?attrs)
          items

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
        List_.Item.create ?id ?style ?classes
                          ~attrs:([ a_role ["menuitem"] ]
                                  |> (fun x -> if disabled
                                               then x @ [ a_aria "disabled" ["true"]
                                                        ; a_tabindex (-1) ]
                                               else x @ [ a_tabindex 0 ])
                                  |> (fun x -> x @ (CCOpt.get_or ~default:[] attrs)))
                          ~text ?text_id ?text_style ?text_classes ?text_attrs ?secondary_text
                          ?secondary_text_id ?secondary_text_style ?secondary_text_classes ?secondary_text_attrs
                          ?start_detail ?end_detail ()

    end

    let create ?id ?style ?(classes=[]) ?attrs
               ?list_id ?list_style ?list_classes ?list_attrs
               ?(opened=false) ?open_from ~items () =
      div ~a:([ a_class (base_class :: classes
                         |> (fun x -> if opened then x @ [CSS.add_modifier base_class "open"] else x)
                         |> (fun x -> match open_from with
                                      | Some open_from -> x @ [(match open_from with
                                                                | `Top_left     -> "top-left"
                                                                | `Top_right    -> "top-right"
                                                                | `Bottom_left  -> "bottom-left"
                                                                | `Bottom_right -> "bottom-right")
                                                               |> (fun s -> CSS.add_modifier base_class
                                                                                              ("open-from-" ^ s))]
                                      | None           -> x))
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

    let base_class = "mdc-select"

  end

  module Slider = struct

    let base_class             = "mdc-slider"
    let track_container_class  = CSS.add_element base_class "track-container"
    let track_class            = CSS.add_element base_class "track"
    let thumb_container_class  = CSS.add_element base_class "thumb-container"
    let thumb_class            = CSS.add_element base_class "thumb"
    let focus_ring_class       = CSS.add_element base_class "focus-ring"
    let pin_class              = CSS.add_element base_class "pin"
    let pin_value_marker_class = CSS.add_element base_class "pin-value-marker"

    let create ?id ?style ?(classes=[]) ?attrs ?(discrete=false)
               ?label ?(min=0) ?(max=100) ?(value=0) () =
      div ~a:([ a_class (base_class :: classes
                         |> fun x -> if discrete then x @ [CSS.add_modifier base_class "discrete"] else x)
              ; a_tabindex 0
              ; a_role ["slider"]
              ; a_aria "valuemin" [ string_of_int min ]
              ; a_aria "valuemax" [ string_of_int max ]
              ; a_aria "valuenow" [ string_of_int value ]
              ]
              |> (fun x -> match label with
                           | Some label -> x @ [a_aria "label" [label]]
                           | None       -> x)
              |> add_common_attrs ?id ?style ?attrs)
          [ div ~a:([ a_class [track_container_class]])
                [ div ~a:([ a_class [track_class]]) []]
          ; div ~a:([ a_class [thumb_container_class]])
                [ svg ~a:([ Svg.a_class [thumb_class]
                          ; Svg.a_width (21., None)
                          ; Svg.a_height (21., None)])
                      [ Svg.circle ~a:[ Svg.a_cx (10.5, None)
                                      ; Svg.a_cy (10.5, None)
                                      ; Svg.a_r (7.875, None)] []]]
          ; div ~a:([ a_class [focus_ring_class]]) []
          ]

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
