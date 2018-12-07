open Utils
open Containers

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class = "mdc-grid-list"
  let tiles_class = CSS.add_element base_class "tiles"
  let tile_gutter_1_class = CSS.add_modifier base_class "tile-gutter-1"
  let icon_align_start_class = CSS.add_modifier base_class "with-icon-align-start"
  let icon_align_end_class = CSS.add_modifier base_class "with-icon-align-end"
  let header_caption_class = CSS.add_modifier base_class "header-caption"
  let twoline_caption_class = CSS.add_modifier base_class "twoline-caption"

  module Tile = struct

    let _class = "mdc-grid-tile"

    module Primary = struct

      let primary_class = CSS.add_element _class "primary"
      let content_class = CSS.add_element _class "primary-content"

      let create_content ?(classes = []) ?attrs
            ?src ?alt ?(is_div = false) () : 'a elt =
        if not is_div
        then img ~src:(Html.uri_of_string (Option.get_or ~default:"" src))
               ~alt:(Option.get_or ~default:"" alt)
               ~a:([a_class (content_class :: classes)] <@> attrs) ()
        else
          let style = match src with
            | None -> None
            | Some x -> Some (Printf.sprintf "background-image: url(%s);" x) in
          div ~a:([a_class (content_class :: classes)]
                  |> map_cons_option a_style style
                  <@> attrs) []

      let create ?(classes = []) ?attrs ~content () : 'a elt =
        div ~a:([a_class (primary_class :: classes)] <@> attrs) [content]

    end

    module Caption = struct

      let secondary_class = CSS.add_element _class "secondary"
      let icon_class = CSS.add_element _class "icon"
      let title_class = CSS.add_element _class "title"
      let support_text_class = CSS.add_element _class "support-text"

      let create_title ?(classes = []) ?attrs ~text () : 'a elt =
        span ~a:([a_class (title_class :: classes)] <@> attrs) [txt text]

      let create_support_text ?(classes = []) ?attrs ~text () : 'a elt =
        span ~a:([a_class (support_text_class :: classes)] <@> attrs)
          [txt text]

      let create ?(classes = []) ?attrs
            ?title ?support_text ?icon () : 'a elt =
        span ~a:([a_class (secondary_class :: classes)] <@> attrs)
          (cons_option support_text []
           |> cons_option title
           |> cons_option icon)

    end

    let create ?(classes = []) ?attrs ?caption ~primary () : 'a elt =
      li ~a:([a_class (_class :: classes)] <@> attrs)
        (cons_option caption [] |> List.cons primary)

  end

  type ar =
    [ `AR_1_1
    | `AR_2_3
    | `AR_3_2
    | `AR_4_3
    | `AR_3_4
    | `AR_16_9
    ]

  let ar_to_string : ar -> string = function
    | `AR_1_1 -> "1x1"
    | `AR_2_3 -> "2x3"
    | `AR_3_2 -> "3x2"
    | `AR_4_3 -> "4x3"
    | `AR_3_4 -> "3x4"
    | `AR_16_9 -> "16x9"

  let ar_to_class (x : ar) : string =
    CSS.add_modifier base_class ("tile-aspect-" ^ ar_to_string x)

  let create ?(classes = []) ?attrs
        ?ar ?(one_px_gutter = false) ?(header_caption = false)
        ?(twoline = false) ?icon_align ~tiles () : 'a elt =
    let classes =
      base_class :: classes
      |> map_cons_option ar_to_class ar
      |> cons_if one_px_gutter  tile_gutter_1_class
      |> cons_if header_caption header_caption_class
      |> cons_if twoline        twoline_caption_class
      |> map_cons_option (function
             | `Start -> icon_align_start_class
             | `End   -> icon_align_end_class)
           icon_align in
    div ~a:([a_class classes] <@> attrs)
      [ul ~a:[ a_class [tiles_class] ] tiles]

end
