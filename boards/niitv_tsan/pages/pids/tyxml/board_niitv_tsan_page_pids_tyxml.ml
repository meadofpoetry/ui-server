open Components_tyxml
open Application_types
open Board_niitv_tsan_widgets_tyxml
open Board_niitv_tsan_types

let id control = Printf.sprintf "board-%d-pids" control

module CSS = struct
  let root = Printf.sprintf "%s-page-pids" (Topology.make_dom_node_class board_id)
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Layout_grid_markup = Layout_grid.Make (Xml) (Svg) (Html)
  module Pid_bitrate_pie_chart_markup = Pid_bitrate_pie_chart.Make (Xml) (Svg) (Html)
  module Bitrate_summary_markup = Bitrate_summary.Make (Xml) (Svg) (Html)
  module Pid_summary_markup = Pid_summary.Make (Xml) (Svg) (Html)
  module Divider_markup = Divider.Make (Xml) (Svg) (Html)

  let create ?(classes = []) ?(a = []) ?children () =
    let classes = CSS.root :: Layout_grid.CSS.inner :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          Layout_grid_markup.
            [ layout_grid_cell
                ~span:4
                ~span_tablet:8
                ~children:[Pid_bitrate_pie_chart_markup.create ()]
                ()
            ; layout_grid_cell
                ~span:8
                ~children:
                  [ Bitrate_summary_markup.create ()
                  ; Divider_markup.divider_hr ()
                  ; Pid_summary_markup.create () ]
                () ]
    in
    div ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
