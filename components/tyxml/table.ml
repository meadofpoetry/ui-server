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

    let create ?(classes = []) ?attrs ?colspan ?(is_numeric = false)
          ?(dense = false) content () : 'a elt =
      let classes =
        classes
        |> cons_if is_numeric numeric_class
        |> cons_if dense dense_class
        |> List.cons _class in
      td ~a:([a_class classes]
             |> map_cons_option a_colspan colspan
             <@> attrs) content
  end

  module Column = struct
    let _class = CSS.add_element  base_class "column"
    let sortable_class = CSS.add_modifier _class "sortable"
    let numeric_class = CSS.add_modifier _class "numeric"
    let dense_class = CSS.add_modifier _class "dense"

    let create ?(classes = []) ?attrs ?(is_numeric = false) ?(sortable = false)
          ?(dense = false) content () : 'a elt =
      let classes =
        classes
        |> cons_if sortable sortable_class
        |> cons_if is_numeric numeric_class
        |> cons_if dense dense_class
        |> List.cons _class in
      th ~a:([a_class classes] <@> attrs) [content]
  end

  module Row = struct
    let _class = CSS.add_element base_class "row"
    let selected_class = CSS.add_modifier _class "selected"
    let disabled_class = CSS.add_modifier _class "disabled"
    let create ?(classes = []) ?attrs ~cells () : 'a elt =
      tr ~a:([a_class (_class :: classes)] <@> attrs) cells
  end

  module Header = struct
    let create ?(classes = []) ?attrs ~row () : 'a elt =
      thead ~a:([a_class classes] <@> attrs) [row]
  end

  module Body = struct
    let create ?(classes = []) ?attrs ~rows () : 'a elt =
      tbody ~a:([a_class classes] <@> attrs) rows
  end

  module Footer = struct

    let _class = "mdc-data-table-footer"
    let cell_class = CSS.add_element _class "cell"
    let toolbar_class = CSS.add_element _class "toolbar"
    let spacer_class = CSS.add_element _class "spacer"
    let caption_class = CSS.add_element _class "caption"
    let select_class = CSS.add_element _class "select"
    let actions_class = CSS.add_element _class "actions"

    let create_caption ?(classes = []) ?attrs text () : 'a elt =
      span ~a:([a_class (caption_class :: classes)] <@> attrs) [txt text]

    let create_spacer ?(classes = []) ?attrs () : 'a elt =
      div ~a:([a_class (spacer_class :: classes)] <@> attrs) []

    let create_select ?(classes = []) ?attrs select () =
      div ~a:([a_class (select_class :: classes)] <@> attrs) [select]

    let create_actions ?(classes = []) ?attrs actions () : 'a elt =
      div ~a:([a_class (actions_class :: classes)] <@> attrs) actions

    let create_toolbar ?(classes = []) ?attrs content () : 'a elt =
      div ~a:([a_class (toolbar_class :: classes)] <@> attrs) content

    let create ?(classes = []) ?attrs ~row () : 'a elt =
      tfoot ~a:([a_class (_class :: classes)] <@> attrs) [row]

  end

  let create_table ?(classes = []) ?attrs ?header ?footer ~body () : 'a elt =
    table ?thead:header ?tfoot:footer
      ~a:([a_class (table_class :: classes)] <@> attrs) [body]

  let create_content ?(classes = []) ?attrs ~table () : 'a elt =
    div ~a:([a_class (content_class :: classes)] <@> attrs) [table]

  let create ?(classes = []) ?attrs ?selection ?footer ~content () : 'a elt =
    let selection_class = match selection with
      | None -> None
      | Some `Single -> Some select_class
      | Some `Multiple -> Some select_multiple_class in
    let classes =
      base_class :: (selection_class ^:: classes)
      |> map_cons_option (fun _ -> with_footer_class) footer in
    div ~a:([a_class classes] <@> attrs) (content :: (cons_option footer []))

end
