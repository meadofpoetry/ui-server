open Base

type axis = [`X | `Y | `XY]

let axis_to_string = function
  | `X -> "x" | `Y -> "y" | `XY -> "xy"
let axis_of_string_exn = function
  | "x" -> `X | "y" -> `Y | "xy" -> `XY | _ -> failwith "Bad axis string"

class type t_js =
  object
    method mode              : Js.js_string Js.t Js.prop
    method intersect         : bool Js.t Js.prop
    method axis              : Js.js_string Js.t Js.prop
    method animationDuration : int Js.prop
  end

class t () = object(self)
  inherit [t_js] base_option ()

  (** Sets which elements appear in the tooltip. **)
  method mode : interaction_mode = interaction_mode_of_string_exn @@ Js.to_string obj##.mode
  method set_mode (x:interaction_mode) = obj##.mode := Js.string @@ interaction_mode_to_string x

  (** if true, the hover mode only applies when the mouse position intersects an item on the chart. **)
  method intersect : bool = Js.to_bool obj##.intersect
  method set_intersect x = obj##.intersect := Js.bool x

  (** Can be set to 'x', 'y', or 'xy' to define which directions are used in calculating distances.
   ** Defaults to 'x' for index mode and 'xy' in dataset and nearest modes.
   **)
  method axis : axis = axis_of_string_exn @@ Js.to_string obj##.axis
  method set_axis (x:axis) = obj##.axis := Js.string @@ axis_to_string x

  (** Duration in milliseconds it takes to animate hover style changes. **)
  method animation_duration : int = obj##.animationDuration
  method set_animation_duration x = obj##.animationDuration := x

  initializer
    self#set_mode `Nearest;
    self#set_intersect true;
    self#set_axis `XY;
    self#set_animation_duration 400
end
