open Base

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
  |> Obj.map_cons_option ~f:(Base.interaction_mode_to_string %> Js.string) "mode" mode
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
