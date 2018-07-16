open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

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

    let create ?(classes=[]) ?attrs ?span ?align ?order ~content () =
      div ~a:([ a_class (_class :: classes
                         |> map_cons_option (fun x -> get_cell_span ?device_type:x.device_type
                                                        x.columns) span
                         |> map_cons_option get_cell_align align
                         |> map_cons_option get_cell_order order) ]
              <@> attrs) content

  end

  let create_inner ?(classes=[]) ?attrs ~cells () =
    div ~a:([ a_class (inner_class :: classes)] <@> attrs) cells

  let create ?(classes=[]) ?attrs ?align ?(fixed_column_width=false) ~content () =
    div ~a:([ a_class (classes
                       |> map_cons_option get_grid_align align
                       |> cons_if fixed_column_width fixed_column_width_class
                       |> List.cons base_class) ] <@> attrs) content

end
