open Base

type position = Top | Left | Bottom | Right

class type item =
  object
    method text           : Js.js_string Js.t Js.prop
    method fillStyle      : Js.js_string Js.t Js.prop (* FIXME to color type *)
    method hidden         : bool Js.t Js.prop
    method lineCap        : Js.js_string Js.t Js.prop
    method lineDash       : Js.number Js.t Js.js_array Js.t Js.prop
    method lineDashOffset : Js.number Js.t Js.prop
    method lineJoin       : Js.js_string Js.t Js.prop
    method lineWidth      : Js.number Js.t Js.prop
    method strokeStyle    : Js.js_string Js.t Js.prop (* FIXME to color type *)
    method pointStyle     : Js.js_string Js.t Js.prop
  end

class type labels_js =
  object
    inherit Font.t_js
    method boxWidth       : int Js.prop
    method padding        : int Js.prop
    method generateLabels : (unit -> unit) Js.optdef_prop (* FIXME *)
    method filter         : (unit -> unit) Js.optdef_prop (* FIXME *)
    method usePointStyle  : bool Js.t Js.prop
  end

class type t_js =
  object
    method display   : bool Js.t Js.prop
    method position  : Js.js_string Js.t Js.prop
    method fullWidth : bool Js.t Js.prop
    (* method onClick   : (chart Js.t,(Dom_html.event Js.t -> item Js.t)) Js.meth_callback Js.optdef_prop *)
    (* method onHover   : (chart Js.t,(Dom_html.event Js.t -> item Js.t)) Js.meth_callback Js.optdef_prop *)
    method reverse   : bool Js.t Js.prop
    method labels    : labels_js Js.t Js.prop
  end

let position_to_string = function
  | Top -> "top" | Left -> "left" | Bottom -> "bottom" | Right -> "right"
let position_of_string_exn = function
  | "top" -> Top | "left" -> Left | "bottom" -> Bottom | "right" -> Right | _ -> failwith "Bad position string"

class labels () = object
  inherit [labels_js] base_option ()

  method set_box_width x = obj##.boxWidth := x
  method get_box_width   = obj##.boxWidth

  method set_padding x = obj##.padding := x
  method get_padding   = obj##.padding

  method set_use_point_style x = obj##.usePointStyle := Js.bool x
  method get_use_point_style   = Js.to_bool obj##.usePointStyle

  initializer
    obj##.boxWidth := 40;
    obj##.padding := 10;
    obj##.usePointStyle := Js._false
end

class t () = object(self)
  inherit [t_js] base_option () as super
  val labels = new labels ()

  method private position_to_js x = Js.string @@ position_to_string x

  method set_display x = obj##.display := Js.bool x
  method get_display   = Js.to_bool obj##.display

  method set_position x = obj##.position := self#position_to_js x
  method get_position   = position_of_string_exn @@ Js.to_string obj##.position

  method set_full_width x = obj##.fullWidth := Js.bool x
  method get_full_width   = Js.to_bool obj##.fullWidth

  method set_reverse x = obj##.reverse := Js.bool x
  method get_reverse   = Js.to_bool obj##.reverse

  method labels = labels

  method! replace x = super#replace x; labels#replace obj##.labels

  initializer
    self#set_display true;
    (* self#set_position Top; *)
    self#set_full_width true;
    self#set_reverse false;
    obj##.labels := labels#get_obj;
end
