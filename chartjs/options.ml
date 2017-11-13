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

module Obj = struct
  let (>|=) x f = Js.Optdef.map x f
  let map x f   = Js.Optdef.option x >|= f |> Js.Unsafe.inject
  let wrap x    = Js.Optdef.option x |> Js.Unsafe.inject
end

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
    let font_size'   = (f "fontSize",   Obj.wrap font.size) in
    let font_style'  = (f "fontStyle",  Obj.map font.style Js.string) in
    let font_color'  = (f "fontColor",  Obj.map font.color Js.string) in
    let font_family' = (f "fontFamily", Obj.map font.family Js.string) in
    [| font_size'; font_style'; font_color'; font_family' |]

end

module Responsive = struct

  class type t =
    object
      method responsive                  : bool Js.t Js.optdef_prop
      method responsiveAnimationDuration : float Js.optdef_prop
      method maintainAspectRatio         : bool Js.t Js.prop
      method onResize                    : (unit -> unit) Js.prop (* FIXME *)
    end

  let to_obj ?responsive ?animation_duration ?maintain_ar ?on_resize () : t Js.t =
    let responsive'         = ("responsive", Obj.map responsive Js.bool) in
    let animation_duration' = ("responsiveAnimationDuration", Obj.wrap animation_duration) in
    let maintain_ar'        = ("maintainAspectRatio", Obj.map maintain_ar Js.bool) in
    let on_resize'          = ("onResize", Obj.wrap on_resize) in
    Js.Unsafe.obj [| responsive'; animation_duration'; maintain_ar'; on_resize' |]

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
      method mode_              : Js.js_string Js.t Js.prop
      method intersect_         : bool Js.t Js.prop
      method axis_              : Js.js_string Js.t Js.prop
      method animationDuration_ : float Js.prop
    end

  class type events =
    object
      method events_  : Js.js_string Js.t Js.js_array Js.t Js.prop
      method onHover_ : (unit -> unit) Js.prop (* FIXME *)
      method onClick_ : (unit -> unit) Js.prop (* FIXME *)
    end

  let to_hover_obj ?mode ?intersect ?axis ?animation_duration () : hover Js.t =
    let mode'               = ("mode", Obj.map mode (interaction_mode_to_string %> Js.string)) in
    let intersect'          = ("intersect", Obj.map intersect Js.bool) in
    let axis'               = ("axis", Obj.map axis (axis_to_string %> Js.string)) in
    let animation_duration' = ("animationDuration", Obj.wrap animation_duration) in
    Js.Unsafe.obj [| mode'; intersect'; axis'; animation_duration' |]

  let to_events_obj ?events ?on_hover ?on_click () : events Js.t =
    let events'   = ("events", Obj.map events @@ List.map (event_to_string %> Js.string)) in
    let on_hover' = ("onHover", Obj.wrap on_hover) in
    let on_click' = ("onClick", Obj.wrap on_click) in
    Js.Unsafe.obj [| events'; on_hover'; on_click' |]

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
    [| "boxWidth", Obj.wrap box_width
     ; "padding",  Obj.wrap padding
     ; "usePointStyle", Obj.map use_point_style Js.bool |]
    |> (fun x -> match font with
                 | Some font -> Array.append x @@ Font.to_array font
                 | None      -> x)
    |> Js.Unsafe.obj

  let to_obj ?display ?position ?full_width ?on_click ?on_hover ?reverse ?labels () : t Js.t =
    let display'    = ("display",   Obj.map display Js.bool) in
    let position'   = ("position",  Obj.map position @@ position_to_string %> Js.string) in
    let full_width' = ("fullWidth", Obj.map full_width Js.bool) in
    let on_click'   = ("onClick",   Obj.wrap on_click) in
    let on_hover'   = ("onHover",   Obj.wrap on_hover) in
    let reverse'    = ("reverse",   Obj.map reverse Js.bool) in
    let labels'     = ("labels",    Obj.wrap labels) in
    Js.Unsafe.obj [| display'; position'; full_width'; on_click'; on_hover'; reverse'; labels' |]

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
    [| "display",    Obj.map display Js.bool
     ; "position",   Obj.map position @@ Legend.position_to_string %> Js.string
     ; "padding",    Obj.wrap padding
     ; "lineHeight", Obj.wrap line_height
     ; "text",       Obj.map text Js.string |]
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
    [| "enabled",            Obj.map enabled Js.bool
     ; "mode",               Obj.map mode @@ interaction_mode_to_string %> Js.string
     ; "intersect",          Obj.map intersect Js.bool
     ; "position",           Obj.map position @@ position_to_string %> Js.string
     ; "backgroundColor",    Obj.map background_color Js.string
     ; "titleSpacing",       Obj.wrap title_spacing
     ; "titleMarginBottom",  Obj.wrap title_margin_bottom
     ; "bodySpacing",        Obj.wrap body_spacing
     ; "footerSpacing",      Obj.wrap footer_spacing
     ; "footerMarginTop",    Obj.wrap footer_margin_top
     ; "xPadding",           Obj.wrap x_padding
     ; "yPadding",           Obj.wrap y_padding
     ; "caretPadding",       Obj.wrap caret_padding
     ; "caretSize",          Obj.wrap caret_size
     ; "cornerRadius",       Obj.wrap corner_radius
     ; "multiKeyBackground", Obj.map multi_key_background Js.string
     ; "displayColors",      Obj.map display_colors Js.bool
     ; "borderColor",        Obj.map border_color Js.string
     ; "borderWidth",        Obj.wrap border_width |]
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
      [| "radius",           Obj.wrap radius
       ; "pointStyle",       Obj.map point_style @@ point_style_to_string %> Js.string
       ; "backgroundColor",  Obj.map background_color Js.string
       ; "borderWidth",      Obj.wrap border_width
       ; "borderColor",      Obj.map border_color Js.string
       ; "hitRadius",        Obj.wrap hit_radius
       ; "hoverBorderWidth", Obj.wrap hover_border_width |]
      |> Js.Unsafe.obj

  end

  module Line = struct

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
      [| "backgroundColor",  Obj.map background_color Js.string
       ; "borderWidth",      Obj.wrap border_width
       ; "borderColor",      Obj.map border_color Js.string
       ; "borderSkipped",    Obj.map border_skipped @@ skipped_to_string %> Js.string |]
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
      [| "backgroundColor",  Obj.map background_color Js.string
       ; "borderWidth",      Obj.wrap border_width
       ; "borderColor",      Obj.map border_color Js.string |]
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
    method legend   : Legend.t Js.t Js.optdef_prop
    method title    : Title.t Js.t Js.optdef_prop
    method tooltip  : Tooltip.t Js.t Js.optdef_prop
    method elements : Elements.t Js.t Js.optdef_prop
  end

let to_obj ?legend ?title ?tooltip ?elements () : t Js.t =
  [| "legend",   Obj.wrap legend
   ; "title",    Obj.wrap title
   ; "tooltip",  Obj.wrap tooltip
   ; "elements", Obj.wrap elements |]
  |> Js.Unsafe.obj
