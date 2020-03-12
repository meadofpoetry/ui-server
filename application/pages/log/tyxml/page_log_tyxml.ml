open Components_tyxml

let id = "log"

module CSS = struct
  let root = "page-log"

  let cell = BEM.add_element root "cell"
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

  let create ?(classes = return []) ?(a = []) ~data_table () =
    let classes = fmap (fun x -> CSS.root :: Layout_grid.CSS.inner :: x) classes in
    let children =
      Layout_grid_markup.
        [ layout_grid_cell
            ~classes:(return [ CSS.cell ])
            ~span:12
            ~children:(const [ data_table ])
            ()
        ]
    in
    div ~a:(a_class classes :: a) (const children)
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
