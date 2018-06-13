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

  class ['a] t () = object(self)

    inherit ['a t_js] base_option ()
    inherit Axes_cartesian_common.Tick.t ()

    (** if true, scale will include 0 if it is not already included. *)
    method begin_at_zero   = Js.to_bool obj##.beginAtZero
    method set_begin_at_zero x = obj##.beginAtZero := Js.bool x

    (** User defined minimum number for the scale, overrides minimum value from data. *)
    method min : 'a option = Js.Opt.to_option obj##.min
    method set_min (x:'a option) =
      let v = match x with Some x -> Js.some x | None   -> Js.null in
      obj##.min := v

    (** User defined maximum number for the scale, overrides maximum value from data. *)
    method max : 'a option = Js.Opt.to_option obj##.max
    method set_max (x:'a option) =
      let v = match x with Some x -> Js.some x | None   -> Js.null in
      obj##.max := v

    (** Maximum number of ticks and gridlines to show. *)
    method max_ticks_limit : int = obj##.maxTicksLimit
    method set_max_ticks_limit x = obj##.maxTicksLimit := x

    (** User defined fixed step size for the scale. *)
    method step_size : float option = Js.Opt.to_option obj##.stepSize
    method set_step_size x = obj##.stepSize := Js.some x

    (** Adjustment used when calculating the maximum data value. *)
    method suggested_max : 'a option = Js.Opt.to_option obj##.suggestedMax
    method set_suggested_max (x:'a) = obj##.suggestedMax := Js.some x

    (** Adjustment used when calculating the minimum data value. *)
    method suggested_min : 'a option = Js.Opt.to_option obj##.suggestedMin
    method set_suggested_min (x:'a) = obj##.suggestedMin := Js.some x

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
  object(self)
    inherit ['a,'a,'a t_js] Axes_cartesian_common.t ~id ~position ~axis () as super
    val _ticks = new Tick.t ()

    method ticks = _ticks

    method min       = self#ticks#min
    method set_min x = self#ticks#set_min x
    method max       = self#ticks#max
    method set_max x = self#ticks#set_max x

    method! replace x = super#replace x; self#ticks#replace obj##.ticks

    initializer
      obj##.ticks := self#ticks#get_obj
  end
