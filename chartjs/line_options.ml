open Base

class type t_js =
  object
    inherit Options.t_js
    method showLines : bool Js.t Js.prop
    method spanGaps  : bool Js.t Js.prop
    method scales    : Axes.Cartesian.t_js Js.t Js.prop
  end

class t ~(x_axis:(_,'a) #Axes_cartesian_common.t) ~(y_axis:(_,'b) #Axes_cartesian_common.t)
        () =
object

  inherit [t_js] Options.t () as super

  val _scales = new Axes.Cartesian.t ~x_axes:[x_axis] ~y_axes:[y_axis] ()

  (** If false, the lines between points are not drawn. **)
  method show_lines : bool = Js.to_bool obj##.showLines
  method set_show_lines x = obj##.showLines := Js.bool x

  (** If false, NaN data causes a break in the line. **)
  method span_gaps : bool = Js.to_bool obj##.spanGaps
  method set_span_gaps x = obj##.spanGaps := Js.bool x

  method! replace x = super#replace x; _scales#replace obj##.scales

  initializer
    obj##.scales := _scales#get_obj

end
