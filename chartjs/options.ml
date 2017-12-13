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
let event_of_string_exn = function
  | "mousemove"  -> Mousemove  | "mouseout"  -> Mouseout  | "click"    -> Click
  | "touchstart" -> Touchstart | "touchmove" -> Touchmove | "touchend" -> Touchend
  | _ -> failwith "Bad event string"

class type t_js =
  object
    method responsive                  : bool Js.t Js.prop
    method responsiveAnimationDuration : int Js.prop
    method maintainAspectRatio         : bool Js.t Js.prop
    method onResize                    : (unit -> unit) Js.prop (* FIXME *)
    method events                      : Js.js_string Js.t Js.js_array Js.t Js.prop
    method onHover                     : (Dom_html.event Js.t -> 'a Js.js_array Js.t -> unit) Js.meth Js.prop
    method onClick                     : (Dom_html.event Js.t -> 'a Js.js_array Js.t -> unit) Js.meth Js.prop
    method hover                       : Hover.t_js Js.t Js.prop
    method animation                   : Animation.t_js Js.t Js.prop
    method layout                      : Layout.t_js Js.t Js.prop
    method legend                      : Legend.t_js Js.t Js.prop
    method title                       : Title.t_js Js.t Js.prop
    method tooltips                    : Tooltip.t_js Js.t Js.prop
    method elements                    : Elements.t_js Js.t Js.prop
    method scales                      : Axes.Cartesian.t_js Js.t Js.prop
  end

class t () = object(self)
  inherit [t_js] base_option () as super

  val hover     = new Hover.t ()
  val animation = new Animation.t ()
  val layout    = new Layout.t ()
  val legend    = new Legend.t ()
  val title     = new Title.t ()
  val tooltip   = new Tooltip.t ()
  val elements  = new Elements.t ()

  method set_responsive x = obj##.responsive := Js.bool x
  method get_responsive   = Js.to_bool obj##.responsive

  method set_responsive_animation_duration x = obj##.responsiveAnimationDuration := x
  method get_responsive_animation_duration   = obj##.responsiveAnimationDuration

  method set_maintain_aspect_ratio x = obj##.maintainAspectRatio := Js.bool x
  method get_maintain_aspect_ratio   = Js.to_bool obj##.maintainAspectRatio

  method set_events x = obj##.events := Js.array @@ Array.of_list @@ List.map (event_to_string %> Js.string) x
  method get_events   = List.map (Js.to_string %> event_of_string_exn) @@ Array.to_list @@ Js.to_array obj##.events

  method hover     = hover
  method animation = animation
  method layout    = layout
  method legend    = legend
  method title     = title
  method tooltip   = tooltip
  method elements  = elements

  method! replace x = super#replace x;
                      hover#replace     obj##.hover;
                      animation#replace obj##.animation;
                      layout#replace    obj##.layout;
                      legend#replace    obj##.legend;
                      title#replace     obj##.title;
                      tooltip#replace   obj##.tooltips;
                      elements#replace  obj##.elements

  initializer
    self#set_responsive true;
    self#set_responsive_animation_duration 0;
    self#set_maintain_aspect_ratio true;
    self#set_events [Mousemove; Mouseout; Click; Touchstart; Touchmove; Touchend];
    obj##.hover     := hover#get_obj;
    obj##.animation := animation#get_obj;
    obj##.layout    := layout#get_obj;
    obj##.legend    := legend#get_obj;
    obj##.title     := title#get_obj;
    obj##.tooltips  := tooltip#get_obj;
    obj##.elements  := elements#get_obj;
end
