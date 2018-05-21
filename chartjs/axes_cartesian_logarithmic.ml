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

  class ['a] t () = object
    inherit ['a t_js] base_option ()
    inherit Axes_cartesian_common.Tick.t ()

    method max : 'a option = Js.Opt.to_option obj##.max
    method set_max (x:'a option) =
      let v = match x with Some x -> Js.some x | None -> Js.null in
      obj##.max := v

    method min : 'a option = Js.Opt.to_option obj##.min
    method set_min (x:'a option) =
      let v = match x with Some x -> Js.some x | None -> Js.null in
      obj##.min := v

  end

end

class type ['a] t_js =
  object
    inherit Axes_cartesian_common.t_js
    method ticks : 'a Tick.t_js Js.t Js.prop
  end

class ['a] t ~id ~position ~(delta:'a option) ~(typ:'a numeric) () =
  let axis = Logarithmic (typ,delta) in
  object(self)
    inherit ['a t_js,'a] Axes_cartesian_common.t ~axis ~id ~position () as super
    val ticks = new Tick.t ()

    method min       = self#ticks#min
    method set_min x = self#ticks#set_min x
    method max       = self#ticks#max
    method set_max x = self#ticks#set_max x

    method ticks = ticks
    method! replace x = super#replace x; ticks#replace obj##.ticks

    initializer
      obj##.ticks := ticks#get_obj
  end
