open Markup
open Tyxml_js

let get_elevation_class = Elevation.get_elevation_class

let remove_elevation elt = List.iter (fun x -> if CCString.prefix ~pre:Elevation.base_class x
                                               then elt#remove_class x)
                                     elt#classes

let set_elevation elt x = remove_elevation elt;
                          elt#add_class Elevation.transition_class;
                          elt#add_class @@ get_elevation_class x
