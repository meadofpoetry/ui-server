open Containers
open Components

let input_to_string : Common.Topology.input -> string = function
  | RF    -> "RF"
  | ASI   -> "ASI"
  | TSOIP -> "TSoIP"

let circle_markup text =
  let open Tyxml_js.Html in
  span ~a:[a_class ["topology__input__circle"]]
       [pcdata text]

let label_markup text =
  let open Tyxml_js.Html in
  span ~a:[a_class ["topology__input__label"]]
       [pcdata text]

let markup_ (input:Common.Topology.topo_input) =
  let name = Common.Topology.get_input_name input in
  let open Tyxml_js.Html in
  div ~a:[a_class ["mdc-chip"; "topology__input"]]
      [ div ~a:[a_class ["mdc-chip__text"]] [pcdata name]
      ; i ~a:[a_class ["material-icons mdc-chip__icon mdc-chip__icon--trailing"]] [ pcdata "arrow_forward" ]]


class t ~input () =
  let elt = markup_ input |> Tyxml_js.To_dom.of_element in
  object
    method input = input
    inherit Widget.widget elt ()
  end

let create input =
  new t ~input ()
