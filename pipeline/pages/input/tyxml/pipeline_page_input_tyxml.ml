open Components_tyxml
open Application_types
open Pipeline_widgets_tyxml

let id = "pipeline"

module CSS = struct
  let root = "pipeline-charts"
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
  module Parameter_chart_markup = Parameter_chart.Make (Xml) (Svg) (Html)

  let create ?(classes = return []) ?(a = []) ~stream_select () =
    let classes =
      fmap (fun x -> CSS.root :: Layout_grid.CSS.inner :: x) classes
    in
    let wrap w =
      Layout_grid_markup.layout_grid_cell ~span:12 ~children:(const [ w ]) ()
    in
    let charts =
      [
        wrap @@ Parameter_chart_markup.create ~typ:`Black ();
        wrap @@ Parameter_chart_markup.create ~typ:`Luma ();
        wrap @@ Parameter_chart_markup.create ~typ:`Freeze ();
        wrap @@ Parameter_chart_markup.create ~typ:`Diff ();
        wrap @@ Parameter_chart_markup.create ~typ:`Blocky ();
        wrap @@ Parameter_chart_markup.create ~typ:`Shortt ();
        wrap @@ Parameter_chart_markup.create ~typ:`Moment ();
      ]
    in
    let children = wrap stream_select :: charts in
    div ~a:(a_class classes :: a) (const children)
end
