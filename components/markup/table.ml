open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg   : Svg_sigs.NoWrap with module Xml := Xml)
         (Html  : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  open Html

  let base_class = "mdc-data-table"
  let content_class = CSS.add_element base_class "content"
  let table_class = CSS.add_element base_class "table"
  let select_class = CSS.add_modifier base_class "select"
  let select_multiple_class = CSS.add_modifier base_class "select-multiple"
  let dense_class = CSS.add_modifier base_class "dense"
  let sticky_header_class = CSS.add_modifier base_class "sticky-header"
  let with_footer_class = CSS.add_modifier base_class "with-footer"

  module Cell = struct
    let _class = CSS.add_element  base_class "cell"
    let numeric_class  = CSS.add_modifier _class "numeric"
    let dense_class = CSS.add_modifier _class "dense"

    let create ?(classes=[])
          ?attrs
          ?colspan
          ?(is_numeric = false)
          ?(dense = false)
          content () =
      td ~a:([ a_class (classes
                        |> cons_if is_numeric numeric_class
                        |> cons_if dense dense_class
                        |> List.cons _class) ]
             |> map_cons_option a_colspan colspan
             <@> attrs) content
  end

  module Column = struct
    let _class = CSS.add_element  base_class "column"
    let sortable_class = CSS.add_modifier _class "sortable"
    let numeric_class = CSS.add_modifier _class "numeric"
    let dense_class = CSS.add_modifier _class "dense"

    let create ?(classes = [])
          ?attrs
          ?(is_numeric = false)
          ?(sortable = false)
          ?(dense = false)
          content () =
      th ~a:([ a_class (classes
                        |> cons_if sortable sortable_class
                        |> cons_if is_numeric numeric_class
                        |> cons_if dense dense_class
                        |> List.cons _class)]
             <@> attrs) [ content ]
  end

  module Row = struct
    let _class = CSS.add_element base_class "row"
    let selected_class = CSS.add_modifier _class "selected"
    let disabled_class = CSS.add_modifier _class "disabled"
    let create ?(classes = []) ?attrs ~cells () =
      tr ~a:([ a_class (_class :: classes) ] <@> attrs) cells
  end

  module Header = struct
    let create ?(classes = []) ?attrs ~row () =
      thead ~a:([ a_class classes ] <@> attrs) [ row ]
  end

  module Body = struct
    let create ?(classes = []) ?attrs ~rows () =
      tbody ~a:([ a_class classes ] <@> attrs) rows
  end

  module Footer = struct

    let _class = "mdc-data-table-footer"
    let cell_class = CSS.add_element _class "cell"
    let toolbar_class = CSS.add_element _class "toolbar"
    let spacer_class = CSS.add_element _class "spacer"
    let caption_class = CSS.add_element _class "caption"
    let select_class = CSS.add_element _class "select"
    let actions_class = CSS.add_element _class "actions"

    let create_caption ?(classes = []) ?attrs text () =
      span ~a:([ a_class (caption_class :: classes) ] <@> attrs)
        [ pcdata text ]

    let create_spacer ?(classes = []) ?attrs () =
      div ~a:([ a_class (spacer_class :: classes) ] <@> attrs) [ ]

    let create_select ?(classes = []) ?attrs select () =
      div ~a:([ a_class (select_class :: classes) ] <@> attrs)
        [ select ]

    let create_actions ?(classes = []) ?attrs actions () =
      div ~a:([ a_class (actions_class :: classes) ] <@> attrs)
        actions

    let create_toolbar ?(classes = []) ?attrs content () =
      div ~a:([ a_class (toolbar_class :: classes) ] <@> attrs)
        content

    let create ?(classes = []) ?attrs ~row () =
      tfoot ~a:([ a_class (_class :: classes) ] <@> attrs) [ row ]

  end

  let create_table ?(classes=[]) ?attrs ?header ?footer ~body () =
    table ?thead:header ?tfoot:footer
      ~a:([ a_class (table_class :: classes) ] <@> attrs) [ body ]

  let create_content ?(classes = []) ?attrs ~table () =
    div ~a:([ a_class (content_class :: classes) ] <@> attrs)
      [ table ]

  let create ?(classes=[]) ?attrs ?selection ?footer ~content () =
    let selection_class = match selection with
      | Some `Multiple -> Some select_multiple_class
      | Some `Single   -> Some select_class
      | None           -> None
    in
    div ~a:([ a_class (base_class :: (selection_class ^:: classes)
                       |> map_cons_option (fun _ -> with_footer_class) footer) ]
            <@> attrs)
      (content :: (cons_option footer []))

end
