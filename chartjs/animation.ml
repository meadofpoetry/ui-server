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

class type t =
  object
    method duration   : int Js.optdef_prop
    method easing     : Js.js_string Js.t Js.optdef_prop
    method onProgress : (animation Js.t -> unit) Js.meth Js.optdef_prop
    method onComplete : (animation Js.t -> unit) Js.meth Js.optdef_prop
  end

let to_obj ?duration ?easing ?on_progress ?on_complete () =
  Obj.cons_option "duration" duration []
  |> Obj.map_cons_option ~f:(Base.easing_to_string %> Js.string) "easing" easing
  |> Obj.map_cons_option ~f:Js.wrap_callback "onProgress" on_progress
  |> Obj.map_cons_option ~f:Js.wrap_callback "onComplete" on_complete
  |> Array.of_list
  |> Js.Unsafe.obj
