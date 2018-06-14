open Containers
open Base
open Axes_cartesian_common

module Tick = struct

  class type t_js =
    object
      inherit Axes_cartesian_common.Tick.t_js
      method labels : Js.js_string Js.t Js.js_array Js.t Js.opt Js.prop
      method min    : Js.js_string Js.t Js.opt Js.prop
      method max    : Js.js_string Js.t Js.opt Js.prop
    end

  class t ~labels () =
    let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object(self)
      inherit base_option o ()
      inherit Axes_cartesian_common.Tick.t ()

      (** An array of labels to display. *)
      method labels : string list option =
        let o = (Js.Opt.to_option _obj##.labels) in
        Option.map (fun x -> List.map Js.to_string @@ Array.to_list @@ Js.to_array x) o
      method set_labels x =
        let a = List.map Js.string x |> Array.of_list |> Js.array |> Js.some in
        _obj##.labels := a

      (** The minimum item to display. *)
      method min : string option = Option.map Js.to_string @@ Js.Opt.to_option _obj##.min
      method set_min x =
        let v = match x with Some x -> Js.some @@ Js.string x | None -> Js.null in
        _obj##.min := v

      (** The maximum item to display. *)
      method max : string option = Option.map Js.to_string @@ Js.Opt.to_option _obj##.max
      method set_max x =
        let v = match x with Some x -> Js.some @@ Js.string x | None -> Js.null in
        _obj##.max := v

      initializer
        self#set_labels labels
    end

end

class type t_js =
  object
    inherit Axes_cartesian_common.t_js
    method ticks : Tick.t_js Js.t Js.prop
  end

class t ~id ~position ~labels () =
  let axis = Category labels in
  let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
  object(self)
    inherit [string,string] Axes_cartesian_common.t ~axis ~id ~position o () as super
    val _ticks = new Tick.t ~labels ()

    method min       = self#ticks#min
    method set_min x = self#ticks#set_min x
    method max       = self#ticks#max
    method set_max x = self#ticks#set_max x

    method ticks = _ticks
    method! replace x = super#replace x; self#ticks#replace _obj##.ticks

    initializer
      _obj##.ticks := Js.Unsafe.coerce self#ticks#get_obj

  end
