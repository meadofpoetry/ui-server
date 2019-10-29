open Components_tyxml
open Application_types
open Board_niitv_tsan_types
open Board_niitv_tsan_widgets_tyxml

let id control = Printf.sprintf "board-%d-pids" control

module CSS = struct
  let root = Printf.sprintf "%s-page-pids" (Topology.make_dom_node_class board_id)
end

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Xml.Wutils
  open Html
  module Layout_grid_markup = Layout_grid.Make (Xml) (Svg) (Html)
  module Pid_bitrate_pie_chart_markup = Pid_bitrate_pie_chart.Make (Xml) (Svg) (Html)
  module Divider_markup = Divider.Make (Xml) (Svg) (Html)

  let create
      ?(classes = return [])
      ?(a = [])
      ~bitrate_summary
      ~pid_summary
      ~pid_overview
      () =
    let classes = fmap (fun x -> CSS.root :: Layout_grid.CSS.inner :: x) classes in
    let children =
      Layout_grid_markup.
        [ layout_grid_cell
            ~span:4
            ~span_tablet:8
            ~children:(const [Pid_bitrate_pie_chart_markup.create ()])
            ()
        ; layout_grid_cell
            ~span:8
            ~children:
              (const [bitrate_summary; Divider_markup.divider_hr (); pid_summary])
            ()
        ; layout_grid_cell ~span:12 ~children:(const [pid_overview]) () ]
    in
    div ~a:(a_class classes :: a) (const children)
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
