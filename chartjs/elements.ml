open Base

module Point = struct

  type point_style = Circle
                   | Cross
                   | Cross_rot
                   | Dash
                   | Line
                   | Rect
                   | Rect_rounded
                   | Rect_rot
                   | Star
                   | Triangle

  let point_style_to_string = function
    | Circle       -> "circle"      | Cross    -> "cross"   | Cross_rot -> "crossRot"
    | Dash         -> "dash"        | Line     -> "line"    | Rect      -> "rect"
    | Rect_rounded -> "rectRounded" | Rect_rot -> "rectRot" | Star      -> "star"
    | Triangle     -> "triangle"
  let point_style_of_string_exn = function
    | "circle"      -> Circle       | "cross"   -> Cross    | "crossRot" -> Cross_rot
    | "dash"        -> Dash         | "line"    -> Line     | "rect"     -> Rect
    | "rectRounded" -> Rect_rounded | "rectRot" -> Rect_rot | "star"     -> Star
    | "triangle"    -> Triangle     | _ -> failwith "Bad point style string"

  class type t_js =
    object
      method radius           : int Js.prop
      method pointStyle       : Js.js_string Js.t Js.prop
      method backgroundColor  : CSS.Color.js_t Js.prop
      method borderWidth      : int Js.prop
      method borderColor      : CSS.Color.js_t Js.prop
      method hitRadius        : int Js.prop
      method hoverRadius      : int Js.prop
      method hoverBorderWidth : int Js.prop
    end

  class t () = object
    inherit [t_js] base_option ()

    method set_radius x = obj##.radius := x
    method get_radius   = obj##.radius

    method set_point_style x = obj##.pointStyle := Js.string @@ point_style_to_string x
    method get_point_style   = point_style_of_string_exn @@ Js.to_string obj##.pointStyle

    method set_background_color x = obj##.backgroundColor := CSS.Color.js x
    method get_background_color   = CSS.Color.ml obj##.backgroundColor

    method set_border_width x = obj##.borderWidth := x
    method get_border_width   = obj##.borderWidth

    method set_border_color x = obj##.borderColor := CSS.Color.js x
    method get_border_color   = CSS.Color.ml obj##.borderColor

    method set_hit_radius x = obj##.hitRadius := x
    method get_hit_radius   = obj##.hitRadius

    method set_hover_radius x = obj##.hoverRadius := x
    method get_hover_radius   = obj##.hoverRadius

    method set_hover_border_width x = obj##.hoverBorderWidth := x
    method get_hover_border_width   = obj##.hoverBorderWidth

    initializer
      obj <- object%js
               val mutable radius           = 3
               val mutable pointStyle       = Js.string @@ point_style_to_string Circle
               val mutable backgroundColor  = CSS.Color.js @@ CSS.Color.rgb ~a:0.1 0 0 0
               val mutable borderWidth      = 1
               val mutable borderColor      = CSS.Color.js @@ CSS.Color.rgb ~a:0.1 0 0 0
               val mutable hitRadius        = 1
               val mutable hoverRadius      = 4
               val mutable hoverBorderWidth = 1
             end
  end

end

module Line = struct

  type bool_or_string
  type fill = Bool of bool
            | Zero
            | Top
            | Bottom

  let fill_to_js : fill -> bool_or_string Js.t = function
    | Bool b -> Js.Unsafe.coerce @@ Js.bool b
    | Zero   -> Js.Unsafe.coerce @@ Js.string "zero"
    | Top    -> Js.Unsafe.coerce @@ Js.string "top"
    | Bottom -> Js.Unsafe.coerce @@ Js.string "bottom"

  let fill_of_js_exn (x : bool_or_string Js.t) : fill =
    match Cast.to_bool x with
    | Some b -> Bool b
    | None   -> (match CCOpt.get_exn @@ Cast.to_string x with
                 | "zero"   -> Zero
                 | "top"    -> Top
                 | "bottom" -> Bottom
                 | _        -> failwith "Bad fill string")

  class type t_js =
    object
      method tension          : float Js.prop
      method backgroundColor  : CSS.Color.js_t Js.prop
      method borderWidth      : int Js.prop
      method borderColor      : CSS.Color.js_t Js.prop
      method borderCapStyle   : Js.js_string Js.t Js.prop
      method borderDash       : int Js.js_array Js.t Js.prop
      method borderDashOffset : int Js.prop
      method borderJoinStyle  : Js.js_string Js.t Js.prop
      method capBezierPoints  : bool Js.t Js.prop
      method fill             : bool_or_string Js.t Js.prop
      method stepped          : bool Js.t Js.prop
    end

  class t () = object
    inherit [t_js] base_option ()

    method set_tension x = obj##.tension := x
    method get_tension   = obj##.tension

    method set_background_color x = obj##.backgroundColor := CSS.Color.js x
    method get_background_color   = CSS.Color.ml obj##.backgroundColor

    method set_border_width x = obj##.borderWidth := x
    method get_border_width   = obj##.borderWidth

    method set_border_color x = obj##.borderColor := CSS.Color.js x
    method get_border_color   = CSS.Color.ml obj##.borderColor

    method set_border_cap_style x = obj##.borderCapStyle := Js.string @@ Canvas.line_cap_to_string x
    method get_border_cap_style   = Canvas.line_cap_of_string_exn @@ Js.to_string obj##.borderCapStyle

    method set_border_dash x = obj##.borderDash := Js.array @@ Array.of_list x
    method get_border_dash   = Array.to_list @@ Js.to_array obj##.borderDash

    method set_border_dash_offset x = obj##.borderDashOffset := x
    method get_border_dash_offset   = obj##.borderDashOffset

    method set_border_join_style x = obj##.borderJoinStyle := Js.string @@ Canvas.line_join_to_string x
    method get_border_join_style   = Canvas.line_join_of_string_exn @@ Js.to_string obj##.borderJoinStyle

    method set_cap_bezier_points x = obj##.capBezierPoints := Js.bool x
    method get_cap_bezier_points   = Js.to_bool obj##.capBezierPoints

    method set_fill x = obj##.fill := fill_to_js x
    method get_fill   = fill_of_js_exn obj##.fill

    method set_stepped x = obj##.stepped := Js.bool x
    method get_stepped   = Js.to_bool obj##.stepped

    initializer
      obj <- object%js
               val mutable tension          = 0.4
               val mutable backgroundColor  = CSS.Color.js @@ CSS.Color.rgb ~a:0.1 0 0 0
               val mutable borderWidth      = 3
               val mutable borderColor      = CSS.Color.js @@ CSS.Color.rgb ~a:0.1 0 0 0
               val mutable borderCapStyle   = Js.string @@ Canvas.line_cap_to_string Butt
               val mutable borderDash       = Js.array [||]
               val mutable borderDashOffset = 0
               val mutable borderJoinStyle  = Js.string @@ Canvas.line_join_to_string Miter
               val mutable capBezierPoints  = Js._true
               val mutable fill             = fill_to_js @@ Bool true
               val mutable stepped          = Js._false
             end
  end

end

module Rectangle = struct

  type skipped = Top | Left | Bottom | Right

  let skipped_to_string = function
    | Top -> "top" | Left -> "left" | Bottom -> "bottom" | Right -> "right"
  let skipped_of_string_exn = function
    | "top" -> Top | "left" -> Left | "bottom" -> Bottom | "right" -> Right | _ -> failwith "Bad skipped string"

  class type t_js =
    object
      method backgroundColor : CSS.Color.js_t Js.prop
      method borderWidth     : int Js.prop
      method borderColor     : CSS.Color.js_t Js.prop
      method borderSkipped   : Js.js_string Js.t Js.prop
    end

  class t () = object
    inherit [t_js] base_option ()

    method set_background_color x = obj##.backgroundColor := CSS.Color.js x
    method get_background_color   = CSS.Color.ml obj##.backgroundColor

    method set_border_width x = obj##.borderWidth := x
    method get_border_width   = obj##.borderWidth

    method set_border_color x = obj##.borderColor := CSS.Color.js x
    method get_border_color   = CSS.Color.ml obj##.borderColor

    method set_border_skipped x = obj##.borderSkipped := Js.string @@ skipped_to_string x
    method get_border_skipped   = skipped_of_string_exn @@ Js.to_string obj##.borderSkipped

    initializer
      obj <- object%js
               val mutable backgroundColor  = CSS.Color.js @@ CSS.Color.rgb ~a:0.1 0 0 0
               val mutable borderWidth      = 0
               val mutable borderColor      = CSS.Color.js @@ CSS.Color.rgb ~a:0.1 0 0 0
               val mutable borderSkipped    = Js.string @@ skipped_to_string Bottom
             end
  end

end

module Arc = struct

  class type t_js =
    object
      method backgroundColor : CSS.Color.js_t Js.prop
      method borderWidth     : int Js.prop
      method borderColor     : CSS.Color.js_t Js.prop
    end

  class t () = object
    inherit [t_js] base_option ()

    method set_background_color x = obj##.backgroundColor := CSS.Color.js x
    method get_background_color   = CSS.Color.ml obj##.backgroundColor

    method set_border_width x = obj##.borderWidth := x
    method get_border_width   = obj##.borderWidth

    method set_border_color x = obj##.borderColor := CSS.Color.js x
    method get_border_color   = CSS.Color.ml obj##.borderColor

    initializer
      obj <- object%js
               val mutable backgroundColor  = CSS.Color.js @@ CSS.Color.rgb ~a:0.1 0 0 0
               val mutable borderWidth      = 2
               val mutable borderColor      = CSS.Color.js @@ CSS.Color.rgb 255 255 255
             end
  end

end

class type t_js =
  object
    method point     : Point.t_js Js.t Js.prop
    method line      : Line.t_js Js.t Js.prop
    method rectangle : Rectangle.t_js Js.t Js.prop
    method arc       : Arc.t_js Js.t Js.prop
  end

class t () = object
  inherit [t_js] base_option () as super
  val point = new Point.t ()
  val line  = new Line.t ()
  val rect  = new Rectangle.t ()
  val arc   = new Arc.t ()

  method point     = point
  method line      = line
  method rectangle = rect
  method arc       = arc

  method! replace x = super#replace x;
                      point#replace obj##.point;
                      line#replace obj##.line;
                      rect#replace obj##.rectangle;
                      arc#replace obj##.arc

  initializer
    obj <- object%js
             val mutable point     = point#get_obj
             val mutable line      = line#get_obj
             val mutable rectangle = rect#get_obj
             val mutable arc       = arc#get_obj
           end
end
