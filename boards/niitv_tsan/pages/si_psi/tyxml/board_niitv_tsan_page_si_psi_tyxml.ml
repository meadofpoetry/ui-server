open Components_tyxml
open Application_types
open Board_niitv_tsan_types

let id control = Printf.sprintf "board-%d-si-psi" control

module CSS = struct
  let root =
    Printf.sprintf "%s-page-si-psi" (Topology.make_dom_node_class board_id)
end

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  open Xml.W
  open Xml.Wutils
  module Layout_grid_markup = Layout_grid.Make (Xml) (Svg) (Html)

  let create ?(classes = return []) ?(a = []) ~stream_select ~si_psi_overview ()
      =
    let classes =
      fmap (fun x -> CSS.root :: Layout_grid.CSS.inner :: x) classes
    in
    let children =
      Layout_grid_markup.
        [
          layout_grid_cell ~span:12 ~children:(const [ stream_select ]) ();
          layout_grid_cell ~span:12 ~children:(const [ si_psi_overview ]) ();
        ]
    in
    div ~a:(a_class classes :: a) (const children)
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
