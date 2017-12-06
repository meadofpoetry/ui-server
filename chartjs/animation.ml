open Base

class type animation =
  object
    method chart               : Base.chart Js.t Js.prop
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

class t () = object
  inherit [t_js] base_option ()
  initializer
    obj <- object%js
             val mutable duration   = 1000
             val mutable easing     = Js.string @@ easing_to_string (Ease_out Quart)
             val mutable onProgress = Js.null
             val mutable onComplete = Js.null
           end
end
