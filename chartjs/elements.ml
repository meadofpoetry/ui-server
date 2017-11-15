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

  class type t =
    object
      method radius           : int Js.optdef_prop
      method pointStyle       : Js.js_string Js.t Js.optdef_prop
      method backgroundColor  : Js.js_string Js.t Js.optdef_prop
      method borderWidth      : int Js.optdef_prop
      method borderColor      : Js.js_string Js.t Js.optdef_prop
      method hitRadius        : int Js.optdef_prop
      method hoverRadius      : int Js.optdef_prop
      method hoverBorderWidth : int Js.optdef_prop
    end

  let to_obj ?radius ?point_style ?background_color ?border_width
             ?border_color ?hit_radius ?hover_radius ?hover_border_width () : t Js.t =
    Obj.cons_option "radius" radius []
    |> Obj.map_cons_option ~f:(point_style_to_string %> Js.string) "pointStyle" point_style
    |> Obj.map_cons_option ~f:Js.string "backgroundColor" background_color
    |> Obj.cons_option "borderWidth" border_width
    |> Obj.map_cons_option ~f:Js.string "borderColor" border_color
    |> Obj.cons_option "hitRadius" hit_radius
    |> Obj.cons_option "hoverRadius" hover_radius
    |> Obj.cons_option "hoverBorderWidth" hover_border_width
    |> Array.of_list
    |> Js.Unsafe.obj

end

module Line = struct

  type fill = Bool of bool
            | Start
            | End
            | Origin
            | Absolute_index of int
            | Relative_index of signed
   and signed = Plus of int | Minus of int


  let fill_to_any = function
    | Bool b           -> Js.Unsafe.inject @@ Js.bool b
    | Start            -> Js.Unsafe.inject @@ Js.string "start"
    | End              -> Js.Unsafe.inject @@ Js.string "end"
    | Origin           -> Js.Unsafe.inject @@ Js.string "origin"
    | Absolute_index x -> Js.Unsafe.inject @@ Js.number_of_float @@ float_of_int x
    | Relative_index x -> (match x with
                           | Plus x  -> Js.Unsafe.inject @@ Js.string ("+" ^ string_of_int x)
                           | Minus x -> Js.Unsafe.inject @@ Js.string ("-" ^ string_of_int x))

  class type t =
    object
      method tension          : float Js.optdef_prop
      method backgroundColor  : Js.js_string Js.t Js.optdef_prop
      method borderWidth      : int Js.optdef_prop
      method borderColor      : Js.js_string Js.t Js.optdef_prop
      method borderCapStyle   : Js.js_string Js.t Js.optdef_prop
      method borderDash       : int Js.js_array Js.t Js.optdef_prop
      method borderDashOffset : int Js.optdef_prop
      method borderJoinStyle  : Js.js_string Js.t Js.optdef_prop
      method capBezierPoints  : bool Js.t Js.optdef_prop
      method fill             : Js.js_string Js.t Js.optdef_prop
      method stepped          : bool Js.t Js.optdef_prop
    end

  let to_obj ?tension ?background_color ?border_width ?border_color
             ?border_cap_style ?border_dash ?border_dash_offset ?border_join_style
             ?cap_bezier_points ?fill ?stepped () : t Js.t =
    Obj.cons_option "tension" tension []
    |> Obj.map_cons_option ~f:Js.string "backgroundColor" background_color
    |> Obj.cons_option "borderWidth" border_width
    |> Obj.map_cons_option ~f:Js.string "borderColor" border_color
    |> Obj.map_cons_option ~f:(Base.Canvas.line_cap_to_string %> Js.string) "borderCapStyle" border_cap_style
    |> Obj.map_cons_option ~f:(Array.of_list %> Js.array) "borderDash" border_dash
    |> Obj.cons_option "borderDashOffset" border_dash_offset
    |> Obj.map_cons_option ~f:(Base.Canvas.line_join_to_string %> Js.string) "borderJoinStyle" border_join_style
    |> Obj.map_cons_option ~f:Js.bool "capBezierPoints" cap_bezier_points
    |> Obj.map_cons_option ~f:fill_to_any "fill" fill
    |> Obj.map_cons_option ~f:Js.bool "stepped" stepped
    |> Array.of_list
    |> Js.Unsafe.obj

end

module Rectangle = struct

  type skipped = Top | Left | Bottom | Right

  let skipped_to_string = function
    | Top -> "top" | Left -> "left" | Bottom -> "bottom" | Right -> "right"

  class type t =
    object
      method backgroundColor : Js.js_string Js.t Js.optdef_prop
      method borderWidth     : int Js.optdef_prop
      method borderColor     : Js.js_string Js.t Js.optdef_prop
      method borderSkipped   : Js.js_string Js.t Js.optdef_prop
    end

  let to_obj ?background_color ?border_width ?border_color ?border_skipped () : t Js.t =
    Obj.map_cons_option ~f:Js.string "backgroundColor" background_color []
    |> Obj.cons_option "borderWidth" border_width
    |> Obj.map_cons_option ~f:Js.string "borderColor" border_color
    |> Obj.map_cons_option ~f:(skipped_to_string %> Js.string) "borderSkipped" border_skipped
    |> Array.of_list
    |> Js.Unsafe.obj

end

module Arc = struct

  class type t =
    object
      method backgroundColor : Js.js_string Js.t Js.optdef_prop
      method borderWidth     : int Js.optdef_prop
      method borderColor     : Js.js_string Js.t Js.optdef_prop
    end

  let to_obj ?background_color ?border_width ?border_color () : t Js.t =
    Obj.map_cons_option ~f:Js.string "backgroundColor" background_color []
    |> Obj.cons_option "borderWidth" border_width
    |> Obj.map_cons_option ~f:Js.string "borderColor" border_color
    |> Array.of_list
    |> Js.Unsafe.obj

end

class type t =
  object
    method point     : Point.t Js.t Js.optdef_prop
    method line      : Line.t Js.t Js.optdef_prop
    method rectangle : Rectangle.t Js.t Js.optdef_prop
    method arc       : Arc.t Js.t Js.optdef_prop
  end

let to_obj ?point ?line ?rectangle ?arc () : t Js.t =
  Obj.cons_option "point" point []
  |> Obj.cons_option "line" line
  |> Obj.cons_option "rectangle" rectangle
  |> Obj.cons_option "arc" arc
  |> (Array.of_list %> Js.Unsafe.obj)
