open Containers
open Base

module Cartesian = struct

  module Category    = Axes_cartesian_category
  module Linear      = Axes_cartesian_linear
  module Logarithmic = Axes_cartesian_logarithmic
  module Time        = Axes_cartesian_time

  class type t_js =
    object
      method xAxes : 'a. 'a Js.t Js.js_array Js.t Js.prop
      method yAxes : 'a. 'a Js.t Js.js_array Js.t Js.prop
    end

  class t ~(x_axes:#Axes_cartesian_common.t_base list)
          ~(y_axes:#Axes_cartesian_common.t_base list)
          () =
    let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object
      inherit base_option o () as super
      val _x_axes : base_option list = List.map (fun x -> (x :> base_option)) x_axes
      val _y_axes : base_option list = List.map (fun x -> (x :> base_option)) y_axes

      method! replace x =
        super#replace x;
        List.iter2 (fun (x:base_option) y -> x#replace y) _x_axes (Array.to_list @@ Js.to_array _obj##.xAxes);
        List.iter2 (fun (x:base_option) y -> x#replace y) _y_axes (Array.to_list @@ Js.to_array _obj##.yAxes)

      initializer
        _obj##.xAxes := Js.array @@ Array.of_list @@ List.map (fun x -> x#get_obj) _x_axes;
        _obj##.yAxes := Js.array @@ Array.of_list @@ List.map (fun x -> x#get_obj) _y_axes
    end

end

module Radial = struct

end
