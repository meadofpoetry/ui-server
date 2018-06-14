open Containers
open Base
open Axes_cartesian_common

module Tick = struct

  class type ['a] t_js =
    object
      inherit Axes_cartesian_common.Tick.t_js
      method beginAtZero   : bool Js.t Js.prop
      method min           : 'a Js.opt Js.prop
      method max           : 'a Js.opt Js.prop
      method maxTicksLimit : int Js.prop
      method stepSize      : float Js.opt Js.prop
      method suggestedMax  : 'a Js.opt Js.prop
      method suggestedMin  : 'a Js.opt Js.prop
    end

  class ['a] t () =
    let o : 'a t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object(self)

      inherit base_option o ()
      inherit Axes_cartesian_common.Tick.t ()

      (** if true, scale will include 0 if it is not already included. *)
      method begin_at_zero   = Js.to_bool _obj##.beginAtZero
      method set_begin_at_zero x = _obj##.beginAtZero := Js.bool x

      (** User defined minimum number for the scale, overrides minimum value from data. *)
      method min : 'a option = Js.Opt.to_option _obj##.min
      method set_min (x:'a option) =
        let v = match x with Some x -> Js.some x | None   -> Js.null in
        _obj##.min := v

      (** User defined maximum number for the scale, overrides maximum value from data. *)
      method max : 'a option = Js.Opt.to_option _obj##.max
      method set_max (x:'a option) =
        let v = match x with Some x -> Js.some x | None   -> Js.null in
        _obj##.max := v

      (** Maximum number of ticks and gridlines to show. *)
      method max_ticks_limit : int = _obj##.maxTicksLimit
      method set_max_ticks_limit x = _obj##.maxTicksLimit := x

      (** User defined fixed step size for the scale. *)
      method step_size : float option = Js.Opt.to_option _obj##.stepSize
      method set_step_size x = _obj##.stepSize := Js.some x

      (** Adjustment used when calculating the maximum data value. *)
      method suggested_max : 'a option = Js.Opt.to_option _obj##.suggestedMax
      method set_suggested_max (x:'a) = _obj##.suggestedMax := Js.some x

      (** Adjustment used when calculating the minimum data value. *)
      method suggested_min : 'a option = Js.Opt.to_option _obj##.suggestedMin
      method set_suggested_min (x:'a) = _obj##.suggestedMin := Js.some x

      initializer
        self#set_begin_at_zero false;
        self#set_max_ticks_limit 11
    end

end

class type ['a] t_js =
  object
    inherit Axes_cartesian_common.t_js
    method ticks : 'a Tick.t_js Js.t Js.prop
  end

class ['a] t ?(delta:'a option) ~id ~position ~(typ:'a numeric) () =
  let axis = Linear (typ,delta) in
  let o : 'a t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
  object(self)
    inherit ['a,'a] Axes_cartesian_common.t ~id ~position ~axis o () as super
    val _ticks = new Tick.t ()

    method ticks = _ticks

    method min       = self#ticks#min
    method set_min x = self#ticks#set_min x
    method max       = self#ticks#max
    method set_max x = self#ticks#set_max x

    method! replace x = super#replace x; self#ticks#replace _obj##.ticks

    initializer
      _obj##.ticks := Js.Unsafe.coerce self#ticks#get_obj
  end
