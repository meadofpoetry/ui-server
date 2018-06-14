open Base

class type t_js =
  object
    inherit Options.t_js
    method showLines : bool Js.t Js.prop
    method spanGaps  : bool Js.t Js.prop
    method scales    : Axes.Cartesian.t_js Js.t Js.prop
  end

class t ~(x_axes:#base_option list) ~(y_axes:#base_option list) () =
  let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
  object
    inherit Options.t o () as super

    val _scales = new Axes.Cartesian.t ~x_axes ~y_axes ()

    (** If false, the lines between points are not drawn. *)
    method show_lines : bool = Js.to_bool _obj##.showLines
    method set_show_lines x = _obj##.showLines := Js.bool x

    (** If false, NaN data causes a break in the line. *)
    method span_gaps : bool = Js.to_bool _obj##.spanGaps
    method set_span_gaps x = _obj##.spanGaps := Js.bool x

    method! replace x = super#replace x; _scales#replace _obj##.scales

    initializer
      _obj##.scales := Js.Unsafe.coerce _scales#get_obj
  end
