open Js_of_ocaml_tyxml
open Application_types

let markup (input : Topology.topo_input) =
  let name = Topology.get_input_name input in
  Tyxml_js.Html.(
    div
      ~a:[a_class ["mdc-chip"; "topology__input"]]
      [ div ~a:[a_class ["mdc-chip__text"]] [txt name]
      ; Components.Icon.SVG.(
          Markup_js.create
            ~size:24
            ~classes:["mdc-chip__icon"; "mdc-chip__icon--trailing"]
            ~d:Path.arrow_right
            ()) ])

class t ~input () =
  let body = Tyxml_js.To_dom.of_element @@ markup input in
  object
    method input = input

    inherit Topo_node.t ~node:(`Entry (Input input)) ~body body ()
  end

let create input = new t ~input ()
