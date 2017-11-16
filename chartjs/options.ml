[@@@ocaml.warning "-60"]

open Base

type event = Mousemove
           | Mouseout
           | Click
           | Touchstart
           | Touchmove
           | Touchend

let event_to_string = function
  | Mousemove  -> "mousemove"  | Mouseout  -> "mouseout"  | Click    -> "click"
  | Touchstart -> "touchstart" | Touchmove -> "touchmove" | Touchend -> "touchend"

class type t =
  object
    method responsive                  : bool Js.t Js.optdef_prop
    method responsiveAnimationDuration : int Js.optdef_prop
    method maintainAspectRatio         : bool Js.t Js.prop
    method onResize                    : (unit -> unit) Js.prop (* FIXME *)
    method events                      : Js.js_string Js.t Js.js_array Js.t Js.optdef_prop
    method onHover                     : (Dom_html.event Js.t -> 'a Js.js_array Js.t -> unit) Js.meth Js.optdef_prop
    method onClick                     : (Dom_html.event Js.t -> 'a Js.js_array Js.t -> unit) Js.meth Js.optdef_prop
    method hover                       : Hover.t Js.t Js.optdef_prop
    method animation                   : Animation.t Js.t Js.optdef_prop
    method layout                      : Layout.t Js.t Js.optdef_prop
    method legend                      : Legend.t Js.t Js.optdef_prop
    method title                       : Title.t Js.t Js.optdef_prop
    method tooltip                     : Tooltip.t Js.t Js.optdef_prop
    method elements                    : Elements.t Js.t Js.optdef_prop
  end

let to_obj ?hover ?animation ?layout ?legend ?title ?tooltips ?elements
           ?responsive ?responsive_animation_duration ?maintain_ar ?on_resize
           ?events ?on_hover ?on_click ?scales () : t Js.t =
  Obj.cons_option "hover" hover []
  |> Obj.cons_option "animation" animation
  |> Obj.cons_option "layout" layout
  |> Obj.cons_option "legend" legend
  |> Obj.cons_option "title" title
  |> Obj.cons_option "tooltips" tooltips
  |> Obj.cons_option "elements" elements
  |> Obj.cons_option "scales" scales
  |> Obj.map_cons_option ~f:Js.bool "responsive" responsive
  |> Obj.cons_option "responsiveAnimationDuration" responsive_animation_duration
  |> Obj.map_cons_option ~f:Js.bool "maintainAspectRatio" maintain_ar
  |> Obj.cons_option "onResize" on_resize
  |> Obj.map_cons_option ~f:(List.map (event_to_string %> Js.string)) "events" events
  |> Obj.map_cons_option ~f:Js.wrap_callback "onHover" on_hover
  |> Obj.map_cons_option ~f:Js.wrap_callback "onClick" on_click
  |> (Array.of_list %> Js.Unsafe.obj)
