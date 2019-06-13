module CSS = struct
  let root = "mdc-data-table"
  let content = BEM.add_element root "content"
  let table = BEM.add_element root "table"
  let cell = BEM.add_element root "cell"
  let column = BEM.add_element root "column"
  let row = BEM.add_element root "row"
  let footer = "mdc-data-table-footer"
  let footer_cell = BEM.add_element footer "cell"
  let footer_toolbar = BEM.add_element footer "toolbar"
  let footer_spacer = BEM.add_element footer "spacer"
  let footer_caption = BEM.add_element footer "caption"
  let footer_select = BEM.add_element footer "select"
  let footer_actions = BEM.add_element footer "actions"

  let select = BEM.add_modifier root "select"
  let select_multiple = BEM.add_modifier root "select-multiple"
  let dense = BEM.add_modifier root "dense"
  let sticky_header = BEM.add_modifier root "sticky-header"
  let with_footer = BEM.add_modifier root "with-footer"
  let cell_numeric  = BEM.add_modifier cell "numeric"
  let cell_dense = BEM.add_modifier cell "dense"
  let column_sortable = BEM.add_modifier column "sortable"
  let column_numeric = BEM.add_modifier column "numeric"
  let column_dense = BEM.add_modifier column "dense"
  let row_selected = BEM.add_modifier row "selected"
  let row_disabled = BEM.add_modifier row "disabled"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create_cell ?(classes = []) ?attrs ?colspan ?(is_numeric = false)
        ?(dense = false) content () : 'a elt =
    let classes =
      classes
      |> cons_if is_numeric CSS.cell_numeric
      |> cons_if dense CSS.cell_dense
      |> List.cons CSS.cell in
    td ~a:([a_class classes]
           |> map_cons_option a_colspan colspan
           <@> attrs) content

  let create_column ?(classes = []) ?attrs
        ?(is_numeric = false) ?(sortable = false)
        ?(dense = false) content () : 'a elt =
    let classes =
      classes
      |> cons_if sortable CSS.column_sortable
      |> cons_if is_numeric CSS.column_numeric
      |> cons_if dense CSS.column_dense
      |> List.cons CSS.column in
    th ~a:([a_class classes] <@> attrs) [content]

  let create_row ?(classes = []) ?attrs ~cells () : 'a elt =
    let classes = CSS.row :: classes in
    tr ~a:([a_class classes] <@> attrs) cells

  let create_header ?(classes = []) ?attrs ~row () : 'a elt =
    thead ~a:([a_class classes] <@> attrs) [row]

  let create_body ?(classes = []) ?attrs ~rows () : 'a elt =
    tbody ~a:([a_class classes] <@> attrs) rows

  let create_footer_caption ?(classes = []) ?attrs text () : 'a elt =
    let classes = CSS.footer_caption :: classes in
    span ~a:([a_class classes] <@> attrs) [txt text]

  let create_footer_spacer ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.footer_spacer :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create_footer_select ?(classes = []) ?attrs select () =
    let classes = CSS.footer_select :: classes in
    div ~a:([a_class classes] <@> attrs) [select]

  let create_footer_actions ?(classes = []) ?attrs actions () : 'a elt =
    let classes = CSS.footer_actions :: classes in
    div ~a:([a_class classes] <@> attrs) actions

  let create_footer_toolbar ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.footer_toolbar :: classes in
    div ~a:([a_class classes] <@> attrs) content

  let create_footer ?(classes = []) ?attrs ~row () : 'a elt =
    let classes = CSS.footer :: classes in
    tfoot ~a:([a_class classes] <@> attrs) [row]

  let create_table ?(classes = []) ?attrs ?header ?footer ~body () : 'a elt =
    let classes = CSS.table :: classes in
    table ?thead:header ?tfoot:footer ~a:([a_class classes] <@> attrs) [body]

  let create_content ?(classes = []) ?attrs ~table () : 'a elt =
    let classes = CSS.content :: classes in
    div ~a:([a_class classes] <@> attrs) [table]

  let create ?(classes = []) ?attrs ?selection ?footer ~content () : 'a elt =
    let selection_class = match selection with
      | None -> None
      | Some `Single -> Some CSS.select
      | Some `Multiple -> Some CSS.select_multiple in
    let classes =
      CSS.root :: (selection_class ^:: classes)
      |> map_cons_option (fun _ -> CSS.with_footer) footer in
    div ~a:([a_class classes] <@> attrs) (content :: (footer ^:: []))

end
