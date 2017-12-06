open Base

type axis = X | Y | XY

let axis_to_string = function
  | X -> "x" | Y -> "y" | XY -> "xy"
let axis_of_string_exn = function
  | "x" -> X | "y" -> Y | "xy" -> XY | _ -> failwith "Bad axis string"

class type t_js =
  object
    method mode              : Js.js_string Js.t Js.prop
    method intersect         : bool Js.t Js.prop
    method axis              : Js.js_string Js.t Js.prop
    method animationDuration : int Js.prop
  end

class t () = object(self)
  inherit [t_js] base_option ()

  method private mode_to_js x = Js.string @@ interaction_mode_to_string x
  method private axis_to_js x = Js.string @@ axis_to_string x

  method set_mode x = obj##.mode := self#mode_to_js x
  method get_mode   = interaction_mode_of_string_exn @@ Js.to_string obj##.mode

  method set_intersect x = obj##.intersect := Js.bool x
  method get_intersect   = Js.to_bool obj##.intersect

  method set_axis x = obj##.axis := self#axis_to_js x
  method get_axis   = axis_of_string_exn @@ Js.to_string obj##.axis

  method set_animation_duration x = obj##.animationDuration := x
  method get_animation_duration   = obj##.animationDuration

  initializer
    obj <- object%js
             val mutable mode = self#mode_to_js Nearest
             val mutable intersect = Js._true
             val mutable axis = self#axis_to_js X
             val mutable animationDuration = 400
           end
end
