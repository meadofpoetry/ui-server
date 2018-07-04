open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg   : Svg_sigs.NoWrap with module Xml := Xml)
         (Html  : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  open Html

  let base_class            = "mdc-data-table"
  let content_class         = CSS.add_element base_class "content"
  let select_class          = CSS.add_modifier base_class "select"
  let select_multiple_class = CSS.add_modifier base_class "select-multiple"

  module Cell = struct
    let _class         = CSS.add_element  base_class "cell"
    let numeric_class  = CSS.add_modifier _class "numeric"
    let dense_class    = CSS.add_modifier _class "dense"

    let create ?(classes=[]) ?attrs
          ?(is_numeric=false) ?(dense=false) content () =
      td ~a:([ a_class (classes
                        |> cons_if is_numeric numeric_class
                        |> cons_if dense dense_class
                        |> List.cons _class)]
             <@> attrs) [ content ]
  end

  module Column = struct
    let _class         = CSS.add_element  base_class "column"
    let sortable_class = CSS.add_modifier _class "sortable"
    let numeric_class  = CSS.add_modifier _class "numeric"
    let dense_class    = CSS.add_modifier _class "dense"

    let create ?(classes=[]) ?attrs
          ?(is_numeric=false) ?(sortable=false) ?(dense=false) content () =
      th ~a:([ a_class (classes
                        |> cons_if sortable sortable_class
                        |> cons_if is_numeric numeric_class
                        |> cons_if dense dense_class
                        |> List.cons _class)]
             <@> attrs) [ content ]
  end

  module Row = struct
    let _class         = CSS.add_element base_class "row"
    let selected_class = CSS.add_modifier _class "selected"
    let disabled_class = CSS.add_modifier _class "disabled"
    let create ?(classes=[]) ?attrs ~cells () =
      tr ~a:([ a_class (_class :: classes) ] <@> attrs) cells
  end

  module Header = struct
    let create ?(classes=[]) ?attrs ~row () =
      thead ~a:([ a_class classes ] <@> attrs) [ row ]
  end

  module Body = struct
    let create ?(classes=[]) ?attrs ~rows () =
      tbody ~a:([ a_class classes ] <@> attrs) rows
  end

  module Footer = struct
    let create ?(classes=[]) ?attrs ~row () =
      tfoot ~a:([ a_class classes ] <@> attrs) [ row ]
  end

  let create_table ?(classes=[]) ?attrs ?header ?footer ~body () =
    table ?thead:header ?tfoot:footer
      ~a:([ a_class (content_class :: classes) ] <@> attrs) [ body ]

  let create ?(classes=[]) ?attrs ?selection ~table () =
    let selection_class = match selection with
      | Some `Multiple -> Some select_multiple_class
      | Some `Single   -> Some select_class
      | None           -> None
    in div ~a:([ a_class (base_class :: (selection_class ^:: classes) )] <@> attrs) [table]

end
