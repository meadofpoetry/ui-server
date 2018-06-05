open Containers
open Base

module Cartesian = struct

  module Category    = Axes_cartesian_category
  module Linear      = Axes_cartesian_linear
  module Logarithmic = Axes_cartesian_logarithmic
  module Time        = Axes_cartesian_time

  class type t_js =
    object
      method xAxes : 'a Js.t Js.js_array Js.t Js.prop
      method yAxes : 'a Js.t Js.js_array Js.t Js.prop
    end

  class t ~(x_axes:'a list) ~(y_axes:'b list) () = object
    inherit [t_js] base_option () as super
    val _x_axes = x_axes
    val _y_axes = y_axes

    method! replace x =
      super#replace x;
      List.iter2 (fun x y -> x#replace y) x_axes (Array.to_list @@ Js.to_array obj##.xAxes);
      List.iter2 (fun x y -> x#replace y) y_axes (Array.to_list @@ Js.to_array obj##.yAxes)

    initializer
      obj##.xAxes := Js.array @@ Array.of_list @@ List.map (fun x -> x#get_obj) _x_axes;
      obj##.yAxes := Js.array @@ Array.of_list @@ List.map (fun x -> x#get_obj) _y_axes
  end

end

module Radial = struct

end
