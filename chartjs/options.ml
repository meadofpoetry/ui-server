[@@@ocaml.warning "-60"]

let (%>) = CCFun.(%>)

type interaction_mode = Point
                      | Nearest
                      | Index
                      | Dataset
                      | X
                      | Y

let interaction_mode_to_string = function
  | Point   -> "point"
  | Nearest -> "nearest"
  | Index   -> "index"
  | Dataset -> "dataset"
  | X       -> "x"
  | Y       -> "y"

module Obj = Base.Obj

module Canvas = struct

  type line_cap = Butt
                | Round
                | Square

  type line_join = Bevel
                 | Round
                 | Miter

  let line_cap_to_string = function
    | Butt -> "butt" | Round -> "round" | Square -> "square"

  let line_join_to_string = function
    | Bevel -> "bevel" | Round -> "round" | Miter -> "miter"

end

module Responsive = struct

  class type t =
    object
      method responsive                  : bool Js.t Js.optdef_prop
      method responsiveAnimationDuration : float Js.optdef_prop
      method maintainAspectRatio         : bool Js.t Js.prop
      method onResize                    : (unit -> unit) Js.prop (* FIXME *)
    end

  let to_array ?responsive ?responsive_animation_duration ?maintain_ar ?on_resize () =
    Obj.map_cons_option ~f:Js.bool "responsive" responsive []
    |> Obj.cons_option "responsiveAnimationDuration" responsive_animation_duration
    |> Obj.map_cons_option ~f:Js.bool "maintainAspectRatio" maintain_ar
    |> Obj.cons_option "onResize" on_resize
    |> Array.of_list

end

module Interaction = struct

  type axis = X
            | Y
            | XY

  type event = Mousemove
             | Mouseout
             | Click
             | Touchstart
             | Touchmove
             | Touchend

  let axis_to_string = function
    | X -> "x" | Y -> "y" | XY -> "xy"

  let event_to_string = function
    | Mousemove  -> "mousemove"  | Mouseout  -> "mouseout"  | Click    -> "click"
    | Touchstart -> "touchstart" | Touchmove -> "touchmove" | Touchend -> "touchend"

  class type hover =
    object
      method mode_              : Js.js_string Js.t Js.optdef_prop
      method intersect_         : bool Js.t Js.optdef_prop
      method axis_              : Js.js_string Js.t Js.optdef_prop
      method animationDuration_ : float Js.optdef_prop
    end

  class type events =
    object
      method events_  : Js.js_string Js.t Js.js_array Js.t Js.optdef_prop
      method onHover_ : (Dom_html.event Js.t -> 'a Js.js_array Js.t -> unit) Js.meth Js.optdef_prop
      method onClick_ : (Dom_html.event Js.t -> 'a Js.js_array Js.t -> unit) Js.meth Js.optdef_prop
    end

  let to_hover_obj ?mode ?intersect ?axis ?animation_duration () =
    Obj.map_cons_option ~f:(interaction_mode_to_string %> Js.string) "mode" mode []
    |> Obj.map_cons_option ~f:Js.bool "intersect" intersect
    |> Obj.map_cons_option ~f:(axis_to_string %> Js.string) "axis" axis
    |> Obj.cons_option "animationDuration" animation_duration
    |> Array.of_list
    |> Js.Unsafe.obj

  let to_events_array ?events ?on_hover ?on_click () =
    Obj.map_cons_option ~f:(List.map (event_to_string %> Js.string)) "events" events []
    |> Obj.map_cons_option ~f:Js.wrap_callback "onHover" on_hover
    |> Obj.map_cons_option ~f:Js.wrap_callback "onClick" on_click
    |> Array.of_list

end

module Font = struct

  type font =
    { family : string option
    ; size   : int option
    ; color  : string option
    ; style  : string option
    }

  class type t =
    object
      method fontSize       : int Js.optdef_prop
      method fontStyle      : Js.js_string Js.t Js.optdef_prop
      method fontColor      : Js.js_string Js.t Js.optdef_prop (* FIXME to color type *)
      method fontFamily     : Js.js_string Js.t Js.optdef_prop
    end

  let to_array ?prefix font =
    let f = (fun x -> match prefix with
                      | None   -> x
                      | Some s -> s ^ (String.capitalize_ascii x)) in
    Obj.cons_option (f "fontSize") font.size []
    |> Obj.map_cons_option ~f:Js.string (f "fontStyle") font.style
    |> Obj.map_cons_option ~f:Js.string (f "fontColor") font.color
    |> Obj.map_cons_option ~f:Js.string (f "fontFamily") font.family
    |> Array.of_list

end

module Animations = struct

  class type animation =
    object
      method chart               : Base.chart Js.t Js.prop
      method currentStep         : Js.number Js.t Js.prop
      method numSteps            : Js.number Js.t Js.prop
      method easing              : Js.js_string Js.t Js.prop
      method render              : unit Js.t Js.prop (* FIXME *)
      method onAnimationProgress : unit Js.t Js.prop (* FIXME *)
      method onAnimationComplete : unit Js.t Js.prop (* FIXME *)
    end

  class type t =
    object
      method duration   : int Js.optdef_prop
      method easing     : Js.js_string Js.t Js.optdef_prop
      method onProgress : (animation Js.t -> unit) Js.meth Js.optdef_prop
      method onComplete : (animation Js.t -> unit) Js.meth Js.optdef_prop
    end

  let to_obj ?duration ?easing ?on_progress ?on_complete () =
    Obj.cons_option "duration" duration []
    |> Obj.map_cons_option ~f:(Base.easing_to_string %> Js.string) "easing" easing
    |> Obj.map_cons_option ~f:Js.wrap_callback "onProgress" on_progress
    |> Obj.map_cons_option ~f:Js.wrap_callback "onComplete" on_complete
    |> Array.of_list
    |> Js.Unsafe.obj

end

module Layout = struct

  type num_or_obj

  type padding = Number of int
               | Object of padding_obj
   and padding_obj =
     { left   : int
     ; right  : int
     ; top    : int
     ; bottom : int
     }

  class type coord =
    object
      method left   : int Js.optdef_prop
      method right  : int Js.optdef_prop
      method top    : int Js.optdef_prop
      method bottom : int Js.optdef_prop
    end

  class type t =
    object
      method padding : num_or_obj Js.t Js.optdef_prop
    end

  let cast_number (x : num_or_obj Js.t)  : Js.number Js.t Js.opt =
    if Js.typeof x = (Js.string "number")
    then Js.some (Js.Unsafe.coerce x)
    else Js.null

  let cast_object (x : num_or_obj Js.t) : coord Js.t Js.opt =
    if Js.typeof x = (Js.string "object")
    then Js.some (Js.Unsafe.coerce x)
    else Js.null

  let to_obj ?padding () =
    (match padding with
     | Some x -> [| "padding", (match x with
                                | Number n -> Js.Unsafe.inject n
                                | Object o -> [| "left",   Js.Unsafe.inject o.left
                                               ; "right",  Js.Unsafe.inject o.right
                                               ; "top",    Js.Unsafe.inject o.top
                                               ; "bottom", Js.Unsafe.inject o.bottom |]
                                              |> Js.Unsafe.obj
                                              |> Js.Unsafe.inject) |]
     | None  -> [| |])
    |> Js.Unsafe.obj

end

module Legend = struct

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

  class type labels =
    object
      inherit Font.t
      method boxWidth       : float Js.optdef_prop
      method padding        : int Js.optdef_prop
      method generateLabels : (unit -> unit) Js.optdef_prop (* FIXME *)
      method filter         : (unit -> unit) Js.optdef_prop (* FIXME *)
      method usePointStyle  : bool Js.t Js.optdef_prop
    end

  class type t =
    object
      method display_   : bool Js.t Js.optdef_prop
      method position_  : Js.js_string Js.t Js.optdef_prop
      method fullWidth_ : bool Js.t Js.optdef_prop
      (* method onClick_   : (chart Js.t,(Dom_html.event Js.t -> item Js.t)) Js.meth_callback Js.optdef_prop *)
      (* method onHover_   : (chart Js.t,(Dom_html.event Js.t -> item Js.t)) Js.meth_callback Js.optdef_prop *)
      method reverse_   : bool Js.t Js.optdef_prop
      method labels_    : labels Js.t Js.optdef_prop
    end

  let position_to_string = function
    | Top -> "top" | Left -> "left" | Bottom -> "bottom" | Right -> "right"

  let labels_to_obj ?box_width ?font ?padding ?use_point_style () : labels Js.t =
    Obj.cons_option "boxWidth" box_width []
    |> Obj.cons_option "padding" padding
    |> Obj.map_cons_option ~f:Js.bool "usePointStyle" use_point_style
    |> Array.of_list
    |> (fun x -> match font with
                 | Some font -> Array.append x @@ Font.to_array font
                 | None      -> x)
    |> Js.Unsafe.obj

  let to_obj ?display ?position ?full_width ?on_click ?on_hover ?reverse ?labels () : t Js.t =
    Obj.map_cons_option ~f:Js.bool "display" display []
    |> Obj.map_cons_option ~f:(position_to_string %> Js.string) "position" position
    |> Obj.map_cons_option ~f:Js.bool "fullWidth" full_width
    |> Obj.cons_option "onClick" on_click
    |> Obj.cons_option "onHover" on_hover
    |> Obj.map_cons_option ~f:Js.bool "reverse" reverse
    |> Obj.cons_option "labels" labels
    |> Array.of_list
    |> Js.Unsafe.obj

end

module Title = struct

  type position = Legend.position

  class type t =
    object
      inherit Font.t
      method display    : bool Js.t Js.optdef_prop
      method position   : Js.js_string Js.t Js.optdef_prop
      method padding    : int Js.optdef_prop
      method lineHeight : float Js.optdef_prop (* FIXME can be a string *)
      method text       : Js.js_string Js.t Js.optdef_prop (* FIXME can be a string array *)
    end

  let to_obj ?display ?position ?font ?padding ?line_height ?text () : t Js.t =
    Obj.map_cons_option ~f:Js.bool "display" display []
    |> Obj.map_cons_option ~f:(Legend.position_to_string %> Js.string) "position" position
    |> Obj.cons_option "padding" padding
    |> Obj.cons_option "lineHeight" line_height
    |> Obj.map_cons_option ~f:Js.string "text" text
    |> Array.of_list
    |> (fun x -> match font with
                 | Some font -> Array.append x @@ Font.to_array font
                 | None      -> x)
    |> Js.Unsafe.obj

end

module Tooltip = struct

  type position = Average | Nearest

  let position_to_string = function
    | Average -> "average" | Nearest -> "nearest"

  class type t =
    object
      method enabled            : bool Js.t Js.optdef_prop
      method mode               : Js.js_string Js.t Js.optdef_prop
      method intersect          : bool Js.t Js.optdef_prop
      method position           : Js.js_string Js.t Js.optdef_prop
      method backgroundColor    : Js.js_string Js.t Js.optdef_prop
      method titleFontFamily    : Js.js_string Js.t Js.optdef_prop
      method titleFontSize      : int Js.optdef_prop
      method titleFontStyle     : Js.js_string Js.t Js.optdef_prop
      method titleFontColor     : Js.js_string Js.t Js.optdef_prop
      method titleSpacing       : int Js.optdef_prop
      method titleMarginBottom  : int Js.optdef_prop
      method bodyFontFamily     : Js.js_string Js.t Js.optdef_prop
      method bodyFontSize       : int Js.optdef_prop
      method bodyFontStyle      : Js.js_string Js.t Js.optdef_prop
      method bodyFontColor      : Js.js_string Js.t Js.optdef_prop
      method bodySpacing        : int Js.optdef_prop
      method footerFontFamily   : Js.js_string Js.t Js.optdef_prop
      method footerFontSize     : int Js.optdef_prop
      method footerFontStyle    : Js.js_string Js.t Js.optdef_prop
      method footerFontColor    : Js.js_string Js.t Js.optdef_prop
      method footerSpacing      : int Js.optdef_prop
      method footerMarginTop    : int Js.optdef_prop
      method xPadding           : int Js.optdef_prop
      method yPadding           : int Js.optdef_prop
      method caretPadding       : int Js.optdef_prop
      method caretSize          : int Js.optdef_prop
      method cornerRadius       : int Js.optdef_prop
      method multiKeyBackground : Js.js_string Js.t Js.optdef_prop
      method displayColors      : bool Js.t Js.optdef_prop
      method borderColor        : Js.js_string Js.t Js.optdef_prop
      method borderWidth        : int Js.optdef_prop
    end

  let to_obj ?enabled ?mode ?intersect ?position ?background_color
             ?title_font ?title_spacing ?title_margin_bottom
             ?body_font ?body_spacing
             ?footer_font ?footer_spacing ?footer_margin_top
             ?x_padding ?y_padding ?caret_padding ?caret_size
             ?corner_radius ?multi_key_background ?display_colors
             ?border_color ?border_width () : t Js.t =
    Obj.map_cons_option ~f:Js.bool "enable" enabled []
    |> Obj.map_cons_option ~f:(interaction_mode_to_string %> Js.string) "mode" mode
    |> Obj.map_cons_option ~f:Js.bool "intersect" intersect
    |> Obj.map_cons_option ~f:(position_to_string %> Js.string) "position" position
    |> Obj.map_cons_option ~f:Js.string "backgroundColor" background_color
    |> Obj.cons_option "titleSpacing" title_spacing
    |> Obj.cons_option "titleMarginBottom" title_margin_bottom
    |> Obj.cons_option "bodySpacing" body_spacing
    |> Obj.cons_option "footerSpacing" footer_spacing
    |> Obj.cons_option "footerMarginTop" footer_margin_top
    |> Obj.cons_option "xPadding" x_padding
    |> Obj.cons_option "yPadding" y_padding
    |> Obj.cons_option "caretPadding" caret_padding
    |> Obj.cons_option "caretSize" caret_size
    |> Obj.cons_option "cornerRadius" corner_radius
    |> Obj.map_cons_option ~f:Js.string "multiKeyBackground" multi_key_background
    |> Obj.map_cons_option ~f:Js.string "displayColors" display_colors
    |> Obj.map_cons_option ~f:Js.string "borderColor" border_color
    |> Obj.cons_option "borderWidth" border_width
    |> Array.of_list
    |> (fun x -> match title_font with
                 | Some font -> Array.append x @@ Font.to_array ~prefix:"title" font
                 | None      -> x)
    |> (fun x -> match body_font with
                 | Some font -> Array.append x @@ Font.to_array ~prefix:"body" font
                 | None      -> x)
    |> (fun x -> match footer_font with
                 | Some font -> Array.append x @@ Font.to_array ~prefix:"footer" font
                 | None      -> x)
    |> Js.Unsafe.obj

end

module Elements = struct

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
        method radius           : float Js.optdef_prop
        method pointStyle       : Js.js_string Js.t Js.optdef_prop
        method backgroundColor  : Js.js_string Js.t Js.optdef_prop
        method borderWidth      : float Js.optdef_prop
        method borderColor      : Js.js_string Js.t Js.optdef_prop
        method hitRadius        : float Js.optdef_prop
        method hoverRadius      : float Js.optdef_prop
        method hoverBorderWidth : float Js.optdef_prop
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
        method borderWidth      : float Js.optdef_prop
        method borderColor      : Js.js_string Js.t Js.optdef_prop
        method borderCapStyle   : Js.js_string Js.t Js.optdef_prop
        method borderDash       : float Js.js_array Js.t Js.optdef_prop
        method borderDashOffset : float Js.optdef_prop
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
      |> Obj.map_cons_option ~f:(Canvas.line_cap_to_string %> Js.string) "borderCapStyle" border_cap_style
      |> Obj.map_cons_option ~f:(Array.of_list %> Js.array) "borderDash" border_dash
      |> Obj.cons_option "borderDashOffset" border_dash_offset
      |> Obj.map_cons_option ~f:(Canvas.line_join_to_string %> Js.string) "borderJoinStyle" border_join_style
      |> Obj.map_cons_option ~f:Js.bool "capBezierPoints" cap_bezier_points
      |> Obj.map_cons_option ~f:fill_to_any "fill" fill
      |> Obj.map_cons_option ~f:Js.bool "stepped" stepped
      |> Array.of_list
      |> Js.Unsafe.obj

  end

  module Rectangle = struct

    type skipped = Legend.position

    let skipped_to_string (x : skipped) = Legend.position_to_string x

    class type t =
      object
        method backgroundColor : Js.js_string Js.t Js.optdef_prop
        method borderWidth     : float Js.optdef_prop
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
        method borderWidth     : float Js.optdef_prop
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

end

class type t =
  object
    inherit Responsive.t
    inherit Interaction.events
    method hover     : Interaction.hover Js.t Js.optdef_prop
    method animation : Animations.t Js.t Js.optdef_prop
    method layout    : Layout.t Js.t Js.optdef_prop
    method legend    : Legend.t Js.t Js.optdef_prop
    method title     : Title.t Js.t Js.optdef_prop
    method tooltip   : Tooltip.t Js.t Js.optdef_prop
    method elements  : Elements.t Js.t Js.optdef_prop
  end

let to_obj ?hover ?animation ?layout ?legend ?title ?tooltips ?elements
           ?responsive ?responsive_animation_duration ?maintain_ar ?on_resize
           ?events ?on_hover ?on_click () : t Js.t =
  Obj.options_to_array [ "hover", hover
                       ; "animation", animation
                       ; "layout", layout
                       ; "legend", legend
                       ; "title", title
                       ; "tooltips", tooltips
                       ; "elements", elements ]
  |> Array.append @@ Interaction.to_events_array ?events ?on_hover ?on_click ()
  |> Array.append @@ Responsive.to_array ?responsive ?responsive_animation_duration ?maintain_ar ?on_resize ()
  |> Js.Unsafe.obj
