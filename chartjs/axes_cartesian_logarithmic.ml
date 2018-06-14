open Containers
open Base
open Axes_cartesian_common

module Tick = struct

  class type ['a] t_js =
    object
      inherit Axes_cartesian_common.Tick.t_js
      method min : 'a Js.opt Js.prop
      method max : 'a Js.opt Js.prop
    end

  class ['a] t () =
    let o : 'a t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object
      inherit base_option o ()
      inherit Axes_cartesian_common.Tick.t ()

      (** User defined maximum number for the scale, overrides maximum value from data. *)
      method max : 'a option = Js.Opt.to_option _obj##.max
      method set_max (x:'a option) =
        let v = match x with Some x -> Js.some x | None -> Js.null in
        _obj##.max := v

      (** User defined minimum number for the scale, overrides minimum value from data. *)
      method min : 'a option = Js.Opt.to_option _obj##.min
      method set_min (x:'a option) =
        let v = match x with Some x -> Js.some x | None -> Js.null in
        _obj##.min := v

  end

end

class type ['a] t_js =
  object
    inherit Axes_cartesian_common.t_js
    method ticks : 'a Tick.t_js Js.t Js.prop
  end

class ['a] t ?(delta:'a option) ~id ~position ~(typ:'a numeric) () =
  let axis = Logarithmic (typ,delta) in
  let o : 'a t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
  object(self)
    inherit ['a,'a] Axes_cartesian_common.t ~axis ~id ~position o () as super
    val _ticks = new Tick.t ()

    method min       = self#ticks#min
    method set_min x = self#ticks#set_min x
    method max       = self#ticks#max
    method set_max x = self#ticks#set_max x

    method ticks = _ticks
    method! replace x = super#replace x; self#ticks#replace _obj##.ticks

    initializer
      _obj##.ticks := Js.Unsafe.coerce self#ticks#get_obj
  end
