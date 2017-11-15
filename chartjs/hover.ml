open Base

type axis = X
          | Y
          | XY

let axis_to_string = function
  | X -> "x" | Y -> "y" | XY -> "xy"

class type t =
  object
    method mode_              : Js.js_string Js.t Js.optdef_prop
    method intersect_         : bool Js.t Js.optdef_prop
    method axis_              : Js.js_string Js.t Js.optdef_prop
    method animationDuration_ : int Js.optdef_prop
  end

let to_obj ?mode ?intersect ?axis ?animation_duration () : t Js.t =
  Obj.map_cons_option ~f:(interaction_mode_to_string %> Js.string) "mode" mode []
  |> Obj.map_cons_option ~f:Js.bool "intersect" intersect
  |> Obj.map_cons_option ~f:(axis_to_string %> Js.string) "axis" axis
  |> Obj.cons_option "animationDuration" animation_duration
  |> Array.of_list
  |> Js.Unsafe.obj
