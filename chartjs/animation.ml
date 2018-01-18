open Base

class type animation =
  object
    (* method chart               : Base.chart Js.t Js.prop *)
    method currentStep         : int Js.prop
    method numSteps            : int Js.prop
    method easing              : Js.js_string Js.t Js.prop
    method render              : unit Js.t Js.prop (* FIXME *)
    method onAnimationProgress : unit Js.t Js.prop (* FIXME *)
    method onAnimationComplete : unit Js.t Js.prop (* FIXME *)
  end

class type t_js =
  object
    method duration   : int Js.prop
    method easing     : Js.js_string Js.t Js.prop
    method onProgress : (animation Js.t -> unit) Js.meth Js.opt Js.prop
    method onComplete : (animation Js.t -> unit) Js.meth Js.opt Js.prop
  end

class t () = object(self)
  inherit [t_js] base_option ()

  method set_duration x = obj##.duration := x
  method get_duration   = obj##.duration

  method set_easing x = obj##.easing := Js.string @@ easing_to_string x
  method get_easing   = easing_of_string_exn @@ Js.to_string obj##.easing

  initializer
    self#set_duration 1000;
    self#set_easing (Ease_out Quart)
end
