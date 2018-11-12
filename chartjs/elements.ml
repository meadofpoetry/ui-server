open Containers
open Base

module Point = struct

  type point_style = [ `Circle
                     | `Cross
                     | `Cross_rot
                     | `Dash
                     | `Line
                     | `Rect
                     | `Rect_rounded
                     | `Rect_rot
                     | `Star
                     | `Triangle
                     ]

  let point_style_to_string = function
    | `Circle       -> "circle"      | `Cross    -> "cross"   | `Cross_rot -> "crossRot"
    | `Dash         -> "dash"        | `Line     -> "line"    | `Rect      -> "rect"
    | `Rect_rounded -> "rectRounded" | `Rect_rot -> "rectRot" | `Star      -> "star"
    | `Triangle     -> "triangle"
  let point_style_of_string_exn = function
    | "circle"      -> `Circle       | "cross"   -> `Cross    | "crossRot" -> `Cross_rot
    | "dash"        -> `Dash         | "line"    -> `Line     | "rect"     -> `Rect
    | "rectRounded" -> `Rect_rounded | "rectRot" -> `Rect_rot | "star"     -> `Star
    | "triangle"    -> `Triangle     | _ -> failwith "Bad point style string"

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

  class t () =
    let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object(self)
      inherit base_option o ()

      (** Point radius. *)
      method radius : int = _obj##.radius
      method set_radius x = _obj##.radius := x

      (** Point style. *)
      method point_style : point_style = point_style_of_string_exn @@ Js.to_string _obj##.pointStyle
      method set_point_style (x:point_style) = _obj##.pointStyle := Js.string @@ point_style_to_string x

      (** Point fill color. *)
      method bg_color : CSS.Color.t = CSS.Color.ml _obj##.backgroundColor
      method set_bg_color x = _obj##.backgroundColor := CSS.Color.js x

      (** Point stroke width. *)
      method border_width : int = _obj##.borderWidth
      method set_border_width x = _obj##.borderWidth := x

      (** Point stroke color. *)
      method border_color : CSS.Color.t = CSS.Color.ml _obj##.borderColor
      method set_border_color x = _obj##.borderColor := CSS.Color.js x

      (** Extra radius added to point radius for hit detection. *)
      method hit_radius : int = _obj##.hitRadius
      method set_hit_radius x = _obj##.hitRadius := x

      (** Point radius when hovered. *)
      method hover_radius : int = _obj##.hoverRadius
      method set_hover_radius x = _obj##.hoverRadius := x

      (** Stroke width when hovered. *)
      method hover_border_width : int = _obj##.hoverBorderWidth
      method set_hover_border_width x = _obj##.hoverBorderWidth := x

      initializer
        self#set_radius 3;
        self#set_point_style `Circle;
        self#set_bg_color @@ CSS.Color.rgb ~a:0.1 0 0 0;
        self#set_border_width 1;
        self#set_border_color @@ CSS.Color.rgb ~a:0.1 0 0 0;
        self#set_hit_radius 1;
        self#set_hover_radius 4;
        self#set_hover_border_width 1;
    end

end

module Line = struct

  type bool_or_string
  type fill = [ `Disabled | `Zero | `Top | `Bottom ]

  let fill_to_js : fill -> bool_or_string Js.t = function
    | `Disabled -> Js.Unsafe.coerce @@ Js._false
    | `Zero     -> Js.Unsafe.coerce @@ Js.string "zero"
    | `Top      -> Js.Unsafe.coerce @@ Js.string "top"
    | `Bottom   -> Js.Unsafe.coerce @@ Js.string "bottom"

  let fill_of_js_exn (x : bool_or_string Js.t) : fill =
    match Cast.to_bool x with
    | Some b -> if b then `Zero else `Disabled
    | None   -> (match Option.get_exn @@ Cast.to_string x with
                 | "zero"   -> `Zero
                 | "top"    -> `Top
                 | "bottom" -> `Bottom
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

  class t () =
    let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object(self)

      inherit base_option o ()

      (** Bézier curve tension (0 for no Bézier curves). *)
      method tension : float = _obj##.tension
      method set_tension x = _obj##.tension := x

      (** Line fill color. *)
      method bg_color : CSS.Color.t = CSS.Color.ml _obj##.backgroundColor
      method set_bg_color x = _obj##.backgroundColor := CSS.Color.js x

      (** Line stroke width. *)
      method border_width : int = _obj##.borderWidth
      method set_border_width x = _obj##.borderWidth := x

      (** Line stroke color. *)
      method border_color : CSS.Color.t = CSS.Color.ml _obj##.borderColor
      method set_border_color x = _obj##.borderColor := CSS.Color.js x

      (** Line cap style *)
      (* method border_cap_style : Canvas.line_cap =
       *   Canvas.line_cap_of_string_exn @@ Js.to_string _obj##.borderCapStyle
       * method set_border_cap_style (x:Canvas.line_cap) =
       *   _obj##.borderCapStyle := Js.string @@ Canvas.line_cap_to_string x *)

      (** Line dash *)
      method border_dash : int list = Array.to_list @@ Js.to_array _obj##.borderDash
      method set_border_dash x = _obj##.borderDash := Js.array @@ Array.of_list x

      (** Line dash offset *)
      method border_dash_offset : int = _obj##.borderDashOffset
      method set_border_dash_offset x = _obj##.borderDashOffset := x

      (** Line join style *)
      (* method border_join_style : Canvas.line_join =
       *   Canvas.line_join_of_string_exn @@ Js.to_string _obj##.borderJoinStyle
       * method set_border_join_style (x:Canvas.line_join) =
       *   _obj##.borderJoinStyle := Js.string @@ Canvas.line_join_to_string x *)

      (** true to keep Bézier control inside the chart, false for no restriction. *)
      method cap_bezier_points : bool = Js.to_bool _obj##.capBezierPoints
      method set_cap_bezier_points x = _obj##.capBezierPoints := Js.bool x

      (** Fill location: 'zero', 'top', 'bottom', true (eq. 'zero') or false (no fill). *)
      method fill : fill = fill_of_js_exn _obj##.fill
      method set_fill x = _obj##.fill := fill_to_js x

      (** true to show the line as a stepped line (tension will be ignored). *)
      method stepped : bool = Js.to_bool _obj##.stepped
      method set_stepped x = _obj##.stepped := Js.bool x

      initializer
        self#set_tension 0.4;
        self#set_bg_color @@ CSS.Color.rgb ~a:0.1 0 0 0;
        self#set_border_width 3;
        self#set_border_color @@ CSS.Color.rgb ~a:0.1 0 0 0;
        (* self#set_border_cap_style `Butt; *)
        self#set_border_dash [];
        self#set_border_dash_offset 0;
        (* self#set_border_join_style `Miter; *)
        self#set_cap_bezier_points true;
        self#set_fill `Zero;
        self#set_stepped false
    end

end

module Rectangle = struct

  type skipped = [`Top | `Left | `Bottom | `Right]

  let skipped_to_string = function
    | `Top    -> "top"
    | `Left   -> "left"
    | `Bottom -> "bottom"
    | `Right  -> "right"
  let skipped_of_string_exn = function
    | "top"    -> `Top
    | "left"   -> `Left
    | "bottom" -> `Bottom
    | "right"  -> `Right
    | _        -> failwith "Bad skipped string"

  class type t_js =
    object
      method backgroundColor : CSS.Color.js_t Js.prop
      method borderWidth     : int Js.prop
      method borderColor     : CSS.Color.js_t Js.prop
      method borderSkipped   : Js.js_string Js.t Js.prop
    end

  class t () =
    let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object(self)
      inherit base_option o ()

      (** Bar fill color. *)
      method bg_color : CSS.Color.t = CSS.Color.ml _obj##.backgroundColor
      method set_bg_color x = _obj##.backgroundColor := CSS.Color.js x

      (** Bar stroke width. *)
      method border_width : int = _obj##.borderWidth
      method set_border_width x = _obj##.borderWidth := x

      (** Bar stroke color. *)
      method border_color : CSS.Color.t = CSS.Color.ml _obj##.borderColor
      method set_border_color x = _obj##.borderColor := CSS.Color.js x

      (** Skipped (excluded) border: 'bottom', 'left', 'top' or 'right'. *)
      method border_skipped : skipped = skipped_of_string_exn @@ Js.to_string _obj##.borderSkipped
      method set_border_skipped (x:skipped) = _obj##.borderSkipped := Js.string @@ skipped_to_string x

      initializer
        self#set_bg_color @@ CSS.Color.rgb ~a:0.1 0 0 0;
        self#set_border_width 0;
        self#set_border_color @@ CSS.Color.rgb ~a:0.1 0 0 0;
        self#set_border_skipped `Bottom
    end

end

module Arc = struct

  class type t_js =
    object
      method backgroundColor : CSS.Color.js_t Js.prop
      method borderWidth     : int Js.prop
      method borderColor     : CSS.Color.js_t Js.prop
    end

  class t () =
    let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
    object(self)
      inherit base_option o ()

      (** Arc fill color. *)
      method bg_color : CSS.Color.t = CSS.Color.ml _obj##.backgroundColor
      method set_bg_color x = _obj##.backgroundColor := CSS.Color.js x

      (** Arc stroke color. *)
      method border_width : int = _obj##.borderWidth
      method set_border_width x = _obj##.borderWidth := x

      (** Arc stroke width. *)
      method border_color : CSS.Color.t = CSS.Color.ml _obj##.borderColor
      method set_border_color x = _obj##.borderColor := CSS.Color.js x

      initializer
        self#set_bg_color @@ CSS.Color.rgb ~a:0.1 0 0 0;
        self#set_border_width 2;
        self#set_border_color @@ CSS.Color.rgb 255 255 255
    end

end

class type t_js =
  object
    method point     : Point.t_js Js.t Js.prop
    method line      : Line.t_js Js.t Js.prop
    method rectangle : Rectangle.t_js Js.t Js.prop
    method arc       : Arc.t_js Js.t Js.prop
  end

class t () =
  let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
  object(self)
    inherit base_option o () as super
    val _point = new Point.t ()
    val _line  = new Line.t ()
    val _rect  = new Rectangle.t ()
    val _arc   = new Arc.t ()

    (** Point elements are used to represent the points in a line chart or a bubble chart. *)
    method point     = _point
    (** Line elements are used to represent the line in a line chart. *)
    method line      = _line
    (** Rectangle elements are used to represent the bars in a bar chart. *)
    method rectangle = _rect
    (** Arcs are used in the polar area, doughnut and pie charts. *)
    method arc       = _arc

    method! replace x = super#replace x;
                        self#point#replace _obj##.point;
                        self#line#replace _obj##.line;
                        self#rectangle#replace _obj##.rectangle;
                        self#arc#replace _obj##.arc

    initializer
      _obj##.point     := Js.Unsafe.coerce self#point#get_obj;
      _obj##.line      := Js.Unsafe.coerce self#line#get_obj;
      _obj##.rectangle := Js.Unsafe.coerce self#rectangle#get_obj;
      _obj##.arc       := Js.Unsafe.coerce self#arc#get_obj
  end
