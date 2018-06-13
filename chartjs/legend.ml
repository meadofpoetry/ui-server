open Base

type position = [ `Top | `Left | `Bottom | `Right ]

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
  | `Top -> "top" | `Left -> "left" | `Bottom -> "bottom" | `Right -> "right"
let position_of_string_exn = function
  | "top" -> `Top | "left" -> `Left | "bottom" -> `Bottom | "right" -> `Right | _ -> failwith "Bad position string"

class labels () = object(self)
  inherit [labels_js] base_option ()
  inherit [labels_js] Font.t { size   = 12
                             ; color  = CSS.Color.rgb 102 102 102
                             ; family = "'Helvetica Neue','Helvetica','Arial',sans-serif"
                             ; style  = `Normal
                             } ()

  (** width of coloured box *)
  method box_width : int = obj##.boxWidth
  method set_box_width x = obj##.boxWidth := x

  (** Padding between rows of colored boxes. *)
  method padding : int = obj##.padding
  method set_padding x = obj##.padding := x

  (** Label style will match corresponding point style (size is based on fontSize,
      boxWidth is not used in this case). *)
  method use_point_style : bool = Js.to_bool obj##.usePointStyle
  method set_use_point_style x = obj##.usePointStyle := Js.bool x

  initializer
    self#set_box_width 40;
    self#set_padding 10;
    self#set_use_point_style false
end

class t () = object(self)
  inherit [t_js] base_option () as super
  val _labels = new labels ()

  (** is the legend shown *)
  method display : bool = Js.to_bool obj##.display
  method set_display x = obj##.display := Js.bool x

  (** Position of the legend. *)
  method position : position = position_of_string_exn @@ Js.to_string obj##.position
  method set_position (x:position) = obj##.position := Js.string @@ position_to_string x

  (** Marks that this box should take the full width of the canvas (pushing down other boxes).
      This is unlikely to need to be changed in day-to-day use. *)
  method full_width : bool = Js.to_bool obj##.fullWidth
  method set_full_width x = obj##.fullWidth := Js.bool x

  (** Legend will show datasets in reverse order. *)
  method reverse : bool = Js.to_bool obj##.reverse
  method set_reverse x = obj##.reverse := Js.bool x

  method labels = _labels

  method! replace x = super#replace x; self#labels#replace obj##.labels

  initializer
    self#set_display true;
    self#set_position `Top;
    self#set_full_width true;
    self#set_reverse false;
    obj##.labels := self#labels#get_obj;
end
