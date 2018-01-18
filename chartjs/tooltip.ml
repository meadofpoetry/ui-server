open Base

type position = Average | Nearest

let position_to_string = function
  | Average -> "average" | Nearest -> "nearest"
let position_of_string_exn = function
  | "average" -> Average | "nearest" -> Nearest | _ -> failwith "Bad position string"

(* FIXME incomplete *)
class type t_js =
  object
    method enabled            : bool Js.t Js.prop
    method mode               : Js.js_string Js.t Js.prop
    method intersect          : bool Js.t Js.prop
    method position           : Js.js_string Js.t Js.prop
    method backgroundColor    : CSS.Color.js_t Js.prop
    method titleFontFamily    : Js.js_string Js.t Js.prop
    method titleFontSize      : int Js.prop
    method titleFontStyle     : Js.js_string Js.t Js.prop
    method titleFontColor     : CSS.Color.js_t Js.prop
    method titleSpacing       : int Js.prop
    method titleMarginBottom  : int Js.prop
    method bodyFontFamily     : Js.js_string Js.t Js.prop
    method bodyFontSize       : int Js.prop
    method bodyFontStyle      : Js.js_string Js.t Js.prop
    method bodyFontColor      : CSS.Color.js_t Js.prop
    method bodySpacing        : int Js.prop
    method footerFontFamily   : Js.js_string Js.t Js.prop
    method footerFontSize     : int Js.prop
    method footerFontStyle    : Js.js_string Js.t Js.prop
    method footerFontColor    : CSS.Color.js_t Js.prop
    method footerSpacing      : int Js.prop
    method footerMarginTop    : int Js.prop
    method xPadding           : int Js.prop
    method yPadding           : int Js.prop
    method caretPadding       : int Js.prop
    method caretSize          : int Js.prop
    method cornerRadius       : int Js.prop
    method multiKeyBackground : CSS.Color.js_t Js.prop
    method displayColors      : bool Js.t Js.prop
    method borderColor        : CSS.Color.js_t Js.prop
    method borderWidth        : int Js.prop
  end

class t () = object(self)
  inherit [t_js] base_option ()
  method set_enabled x = obj##.enabled := Js.bool x
  method get_enabled   = Js.to_bool obj##.enabled

  method set_mode x = obj##.mode := Js.string @@ interaction_mode_to_string x
  method get_mode   = interaction_mode_of_string_exn @@ Js.to_string obj##.mode

  method set_intersect x = obj##.intersect := Js.bool x
  method get_intersect   = Js.to_bool obj##.intersect

  method set_position x = obj##.position := Js.string @@ position_to_string x
  method get_position   = position_of_string_exn @@ Js.to_string obj##.position

  method set_background_color x = obj##.backgroundColor := CSS.Color.js x
  method get_background_color   = CSS.Color.ml obj##.backgroundColor

  method set_title_font_size x = obj##.titleFontSize := x
  method get_title_font_size   = obj##.titleFontSize

  method set_title_font_style (x:Font.style) = obj##.titleFontStyle := Js.string @@ Font.style_to_string x
  method get_title_font_style : Font.style   = Font.style_of_string_exn @@ Js.to_string obj##.titleFontStyle

  method set_title_font_color x = obj##.titleFontColor := CSS.Color.js x
  method get_title_font_color   = CSS.Color.ml obj##.titleFontColor

  method set_title_font_family x = obj##.titleFontFamily := Js.string x
  method get_title_font_family   = Js.to_string obj##.titleFontFamily

  method set_title_spacing x = obj##.titleSpacing := x
  method get_title_spacing   = obj##.titleSpacing

  method set_title_margin_bottom x = obj##.titleMarginBottom := x
  method get_title_margin_bottom   = obj##.titleMarginBottom

  method set_body_font_size x = obj##.bodyFontSize := x
  method get_body_font_size   = obj##.bodyFontSize

  method set_body_font_style (x:Font.style) = obj##.bodyFontStyle := Js.string @@ Font.style_to_string x
  method get_body_font_style : Font.style   = Font.style_of_string_exn @@ Js.to_string obj##.bodyFontStyle

  method set_body_font_color x = obj##.bodyFontColor := CSS.Color.js x
  method get_body_font_color   = CSS.Color.ml obj##.bodyFontColor

  method set_body_font_family x = obj##.bodyFontFamily := Js.string x
  method get_body_font_family   = Js.to_string obj##.bodyFontFamily

  method set_body_spacing x = obj##.bodySpacing := x
  method get_body_spacing   = obj##.bodySpacing

  method set_footer_font_size x = obj##.footerFontSize := x
  method get_footer_font_size   = obj##.footerFontSize

  method set_footer_font_style (x:Font.style) = obj##.footerFontStyle := Js.string @@ Font.style_to_string x
  method get_footer_font_style : Font.style   = Font.style_of_string_exn @@ Js.to_string obj##.footerFontStyle

  method set_footer_font_color x = obj##.footerFontColor := CSS.Color.js x
  method get_footer_font_color   = CSS.Color.ml obj##.footerFontColor

  method set_footer_font_family x = obj##.footerFontFamily := Js.string x
  method get_footer_font_family   = Js.to_string obj##.footerFontFamily

  method set_footer_spacing x = obj##.footerSpacing := x
  method get_footer_spacing   = obj##.footerSpacing

  method set_footer_margin_top x = obj##.footerMarginTop := x
  method get_footer_margin_top   = obj##.footerMarginTop

  method set_x_padding x = obj##.xPadding := x
  method get_x_padding   = obj##.xPadding

  method set_y_padding x = obj##.yPadding := x
  method get_y_padding   = obj##.yPadding

  method set_caret_padding x = obj##.caretPadding := x
  method get_caret_padding   = obj##.caretPadding

  method set_caret_size x = obj##.caretSize := x
  method get_caret_size   = obj##.caretSize

  method set_corner_radius x = obj##.cornerRadius := x
  method get_corner_radius   = obj##.cornerRadius

  method set_multi_key_background x = obj##.multiKeyBackground := CSS.Color.js x
  method get_multi_key_background   = CSS.Color.ml obj##.multiKeyBackground

  method set_display_colors x = obj##.displayColors := Js.bool x
  method get_display_colors   = Js.to_bool obj##.displayColors

  method set_border_color x = obj##.borderColor := CSS.Color.js x
  method get_border_color   = CSS.Color.ml obj##.borderColor

  method set_border_width x = obj##.borderWidth := x
  method get_border_width   = obj##.borderWidth

  initializer
    self#set_enabled true;
    self#set_mode Nearest;
    self#set_intersect true;
    self#set_position Average;
    self#set_background_color @@ CSS.Color.rgb ~a:0.8 0 0 0;
    self#set_title_font_family "'Helvetica Neue','Helvetica','Arial',sans-serif";
    self#set_title_font_size 12;
    self#set_title_font_style `Bold;
    self#set_title_font_color @@ CSS.Color.rgb 255 255 255;
    self#set_title_spacing 2;
    self#set_title_margin_bottom 6;
    self#set_body_font_family "'Helvetica Neue','Helvetica','Arial',sans-serif";
    self#set_body_font_size 12;
    self#set_body_font_style `Normal;
    self#set_body_font_color @@ CSS.Color.rgb 255 255 255;
    self#set_body_spacing 2;
    self#set_footer_font_family "'Helvetica Neue','Helvetica','Arial',sans-serif";
    self#set_footer_font_size 12;
    self#set_footer_font_style `Bold;
    self#set_footer_font_color @@ CSS.Color.rgb 255 255 255;
    self#set_footer_spacing 2;
    self#set_footer_margin_top 6;
    self#set_x_padding 6;
    self#set_y_padding 6;
    self#set_caret_padding 2;
    self#set_caret_size 5;
    self#set_corner_radius 6;
    self#set_multi_key_background @@ CSS.Color.rgb 255 255 255;
    self#set_display_colors true;
    self#set_border_color @@ CSS.Color.rgb ~a:0.0 0 0 0;
    self#set_border_width 0

end
