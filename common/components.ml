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
    val add_modifier  : string -> string -> string

  end = struct

    let concat_one s e d  = s ^ d ^ e

    let add_element s e   = concat_one s e "__"
    let add_modifier s e  = concat_one s e "--"

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

    let base_class     = "mdc-icon"
    let button_class   = CSS.add_modifier base_class "button"
    let disabled_class = CSS.add_modifier base_class "disabled"

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
    let base_class              = "mdc-dynamic-grid"
    let with_overlay_grid_class = CSS.add_modifier base_class "with-overlay-grid"

    module Overlay_grid = struct

      let _class = CSS.add_element base_class "overlay-grid"

      let create ?id ?style ?(classes=[]) ?attrs () =
        canvas ~a:([ a_class (_class :: classes) ]
                   |> add_common_attrs ?id ?style ?attrs)
          []
    end

    module Item = struct

      let _class              = CSS.add_element base_class "item"
      let ghost_class         = CSS.add_element _class "ghost"
      let resize_class        = CSS.add_element _class "resize"
      let selected_class      = CSS.add_modifier _class "selected"
      let dragging_class      = CSS.add_modifier _class "dragging"
      let drag_handle_class   = CSS.add_element _class "drag-handle"
      let select_handle_class = CSS.add_element _class "select-handle"

      let create_ghost ?id ?style ?(classes=[]) ?attrs () =
        div ~a:([ a_class (ghost_class :: classes)]
                |> add_common_attrs ?id ?style ?attrs)
          []

      let create_resize_button ?id ?style ?(classes=[]) ?attrs () =
        span ~a:([ a_class (resize_class :: classes)]
                 |> add_common_attrs ?id ?style ?attrs)
          []

      let create ?id ?style ?(classes=[]) ?attrs ?resize_button () =
        div ~a:([ a_class (_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs)
          (cons_option resize_button [])
    end

    let create ?id ?style ?(classes=[]) ?attrs ~items () =
      div ~a:([ a_class (base_class :: classes) ]
              |> add_common_attrs ?id ?style ?attrs)
        items
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
      let divider_class        = "mdc-list-divider"

      let graphic_class        = CSS.add_element _class "graphic"
      let meta_class           = CSS.add_element _class "meta"

      let (^::) = List.cons_maybe

      let create_divider ?id ?style ?(classes=[]) ?attrs ?(tag=div) ?(inset=false) () =
        tag ~a:([ a_class (classes
                           |> cons_if inset @@ CSS.add_modifier divider_class "inset"
                           |> List.cons divider_class)
                ; a_role ["separator"] ]
                |> add_common_attrs ?id ?style ?attrs)
          []

      let create_secondary_text ?id ?style ?(classes=[]) ?attrs text () =
        span ~a:([ a_class (secondary_text_class :: classes)]
                 |> add_common_attrs ?id ?style ?attrs)
          [ pcdata text]

      let create_text ?id ?style ?(classes=[]) ?attrs ~secondary text () =
        span ~a:([ a_class (text_class :: classes)]
                 |> add_common_attrs ?id ?style ?attrs)
          [ pcdata text; secondary ]

      let create_multi_line ?(tag=li) ?id ?style ?(classes=[]) ?attrs ?graphic ?meta text () =
        tag ~a:([ a_class (_class :: classes)]
                |> add_common_attrs ?id ?style ?attrs)
          (text :: (meta ^:: graphic ^:: []))

      let create_single_line ?(tag=li) ?id ?style ?(classes=[]) ?attrs ?graphic ?meta text () =
        tag ~a:([ a_class (_class :: classes)]
                |> add_common_attrs ?id ?style ?attrs)
          (pcdata text :: (meta ^:: graphic ^:: []))

      (* NOTE legacy *)
      let create ?id ?style ?(classes=[]) ?attrs ?(auto_init=false) ?(tag=li)
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

    let create ?(tag=li) ?id ?style ?(classes=[]) ?attrs
          ?(avatar=false) ?(dense=false) ?(two_line=false) ~items () =
      tag ~a:([ a_class (classes
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

      let _class            = CSS.add_element base_class "item"
      let list_class        = CSS.add_element base_class "list"
      let item_open_class   = CSS.add_modifier _class "open"
      let list_open_class   = CSS.add_modifier list_class "open"

      let create_item = List_.Item.create

      let create_list = List_.create ~classes:[list_class]

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

    let base_class        = "mdc-drawer"
    let temporary_class   = CSS.add_modifier base_class "temporary"
    let drawer_class      = CSS.add_element base_class "drawer"
    let content_class     = CSS.add_element base_class "content"
    let open_class        = CSS.add_modifier base_class "open"
    let animating_class   = CSS.add_modifier base_class "animating"
    let scroll_lock_class = base_class ^ "-scroll-lock"

    module Toolbar_spacer = struct

      let _class = CSS.add_element base_class "toolbar-spacer"

      let create ?id ?style ?(classes=[]) ?attrs ~content () =
        div ~a:([ a_class (_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs)
            content

    end

    module Header = struct

      let _class               = CSS.add_element base_class "header"
      let header_content_class = CSS.add_element base_class "header-content"

      let create ?id ?style ?(classes=[]) ?attrs ~content () =
        header ~a:([ a_class (_class :: classes) ]
                   |> add_common_attrs ?id ?style ?attrs)
               [ div ~a:([ a_class [header_content_class] ])
                     content]

    end

    let create_drawer ?id ?style ?(classes=[]) ?attrs ~content () =
      nav ~a:([ a_class (drawer_class :: classes) ]
              |> add_common_attrs ?id ?style ?attrs)
          content

    let create ?id ?style ?(classes=[]) ?attrs ~drawer () =
      aside ~a:([ a_class (classes
                           |> List.cons Typography.base_class
                           |> List.cons temporary_class
                           |> List.cons base_class) ]
                |> add_common_attrs ?id ?style ?attrs)
            [drawer]

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
                        ((match heading_details with
                          | [] -> None
                          | l  -> Some (div ~a:[ a_class [details_class]] l))
                         |> (fun x -> List.cons_maybe x [])
                         |> List.cons (pcdata title))
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

    let base_class   = "mdc-menu"
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

    let base_class           = "mdc-select"
    let native_control_class = CSS.add_element base_class "native-control"
    let is_changing_class    = CSS.add_modifier base_class "is-changing"
    let disabled_class       = CSS.add_modifier base_class "disabled"

    module Item = struct

      let create ?id ?style ?(classes=[]) ?attrs ?(disabled=false) ?(selected=false) ~text () =
        option ~a:([ a_class classes ]
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

    let create_select ?id ?style ?(classes=[]) ?attrs ?(disabled=false) ~items () =
      select ~a:([ a_class (native_control_class::classes) ]
                 |> cons_if disabled @@ a_disabled ()
                 |> add_common_attrs ?id ?style ?attrs)
             items

    module Label = struct

      let _class            = CSS.add_element base_class "label"
      let float_above_class = CSS.add_modifier _class "float-above"

      let create ?id ?style ?(classes=[]) ?attrs ~label () =
        div ~a:([ a_class (_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs)
            [ pcdata label ]

    end

    module Bottom_line = struct

      let _class       = CSS.add_element base_class "bottom-line"
      let active_class = CSS.add_modifier _class "active"

      let create ?id ?style ?(classes=[]) ?attrs () =
        div ~a:([ a_class (_class :: classes ) ]
                |> add_common_attrs ?id ?style ?attrs)
            []

    end

    let create ?id ?style ?(classes=[]) ?attrs ~select ~label ~bottom_line () =
      div ~a:([ a_class (base_class :: classes)]
              |> add_common_attrs ?id ?style ?attrs)
        [ select; label; bottom_line ]

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

      let create ?id ?style ?(classes=[]) ?attrs ~tabs () =
        div ~a:([ a_class (_class :: classes) ]
                |> add_common_attrs ?id ?style ?attrs)
            [ div ~a:[ a_class [scroll_frame_class]]
                  [ create_indicator ~direction:`Back ()
                  ; div ~a:[ a_class [scroll_frame_tabs_class]
                           ; a_role ["tablist"]] [ tabs ]
                  ; create_indicator ~direction:`Forward ()
                  ]
            ]

    end

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

end
