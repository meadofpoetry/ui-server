open Containers
open Base

type event = [ `Mousemove
             | `Mouseout
             | `Click
             | `Touchstart
             | `Touchmove
             | `Touchend
             ]

let event_to_string = function
  | `Mousemove  -> "mousemove"  | `Mouseout  -> "mouseout"  | `Click    -> "click"
  | `Touchstart -> "touchstart" | `Touchmove -> "touchmove" | `Touchend -> "touchend"
let event_of_string_exn = function
  | "mousemove"  -> `Mousemove  | "mouseout"  -> `Mouseout  | "click"    -> `Click
  | "touchstart" -> `Touchstart | "touchmove" -> `Touchmove | "touchend" -> `Touchend
  | _ -> failwith "Bad event string"

class type t_js =
  object
    method responsive : bool Js.t Js.prop
    method responsiveAnimationDuration : int Js.prop
    method maintainAspectRatio : bool Js.t Js.prop
    method onResize : (unit -> unit) Js.prop (* FIXME *)
    method events : Js.js_string Js.t Js.js_array Js.t Js.prop
    method onHover : (Dom_html.event Js.t -> 'a Js.js_array Js.t -> unit) Js.meth Js.prop
    method onClick : (Dom_html.event Js.t -> 'a Js.js_array Js.t -> unit) Js.meth Js.prop
    method hover : Hover.t_js Js.t Js.prop
    method animation : Animation.t_js Js.t Js.prop
    method layout : Layout.t_js Js.t Js.prop
    method legend : Legend.t_js Js.t Js.prop
    method title : Title.t_js Js.t Js.prop
    method tooltips : Tooltip.t_js Js.t Js.prop
    method elements : Elements.t_js Js.t Js.prop
  end

class t (o:#t_js Js.t) () =
object(self)

  inherit base_option o () as super

  val _hover = new Hover.t ()
  val _animation = new Animation.t ()
  val _layout = new Layout.t ()
  val _legend = new Legend.t ()
  val _title = new Title.t ()
  val _tooltip = new Tooltip.t ()
  val _elements = new Elements.t ()

  (** Resizes the chart canvas when its container does *)
  method responsive : bool = Js.to_bool _obj##.responsive
  method set_responsive x = _obj##.responsive := Js.bool x

  (** Duration in milliseconds it takes to animate to new size after a resize event. *)
  method responsive_animation_duration : int = _obj##.responsiveAnimationDuration
  method set_responsive_animation_duration x = _obj##.responsiveAnimationDuration := x

  (** Maintain the original canvas aspect ratio (width / height) when resizing. *)
  method maintain_aspect_ratio : bool = Js.to_bool _obj##.maintainAspectRatio
  method set_maintain_aspect_ratio x = _obj##.maintainAspectRatio := Js.bool x

  (** The events option defines the browser events that the chart
      should listen to for tooltips and hovering. *)
  method events : event list =
    List.map (Js.to_string %> event_of_string_exn) @@ Array.to_list @@ Js.to_array _obj##.events
  method set_events (x:event list) =
    _obj##.events := Js.array @@ Array.of_list @@ List.map (event_to_string %> Js.string) x

  method hover     = _hover
  method animation = _animation
  method layout    = _layout
  method legend    = _legend
  method title     = _title
  method tooltip   = _tooltip
  method elements  = _elements

  method! replace x =
    super#replace x;
    self#hover#replace     _obj##.hover;
    self#animation#replace _obj##.animation;
    self#layout#replace    _obj##.layout;
    self#legend#replace    _obj##.legend;
    self#title#replace     _obj##.title;
    self#tooltip#replace   _obj##.tooltips;
    self#elements#replace  _obj##.elements;

  initializer
    self#set_responsive true;
    self#set_responsive_animation_duration 0;
    self#set_maintain_aspect_ratio true;
    self#set_events [`Mousemove; `Mouseout; `Click; `Touchstart; `Touchmove; `Touchend];
    _obj##.hover     := Js.Unsafe.coerce self#hover#get_obj;
    _obj##.animation := Js.Unsafe.coerce self#animation#get_obj;
    _obj##.layout    := Js.Unsafe.coerce self#layout#get_obj;
    _obj##.legend    := Js.Unsafe.coerce self#legend#get_obj;
    _obj##.title     := Js.Unsafe.coerce self#title#get_obj;
    _obj##.tooltips  := Js.Unsafe.coerce self#tooltip#get_obj;
    _obj##.elements  := Js.Unsafe.coerce self#elements#get_obj;
end
