open Containers

module Markup = Components_markup.Elevation

let remove_elevation (elt:#Widget.t) =
  List.iter (fun x -> if String.prefix ~pre:Markup.base_class x
                      then elt#remove_class x)
    elt#classes

let set_elevation (elt:#Widget.t) x =
  remove_elevation elt;
  elt#add_class Markup.transition_class;
  elt#add_class @@ Markup.get_elevation_class x
