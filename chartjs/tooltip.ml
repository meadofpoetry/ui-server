open Base

type position = [`Average | `Nearest]

let position_to_string = function
  | `Average -> "average" | `Nearest -> "nearest"
let position_of_string_exn = function
  | "average" -> `Average | "nearest" -> `Nearest | _ -> failwith "Bad position string"

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

class t () =
  let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
  object(self)
    inherit base_option o ()

    (** Are on-canvas tooltips enabled *)
    method enabled : bool = Js.to_bool _obj##.enabled
    method set_enabled x = _obj##.enabled := Js.bool x

    (** Sets which elements appear in the tooltip. *)
    method mode : interaction_mode = interaction_mode_of_string_exn @@ Js.to_string _obj##.mode
    method set_mode (x:interaction_mode) = _obj##.mode := Js.string @@ interaction_mode_to_string x

    (** if true, the tooltip mode applies only when the mouse position intersects with an element.
      If false, the mode will be applied at all times. *)
    method intersect : bool = Js.to_bool _obj##.intersect
    method set_intersect x = _obj##.intersect := Js.bool x

    (** The mode for positioning the tooltip. *)
    method position : position = position_of_string_exn @@ Js.to_string _obj##.position
    method set_position (x:position) = _obj##.position := Js.string @@ position_to_string x

    (** Background color of the tooltip. *)
    method bg_color : CSS.Color.t = CSS.Color.ml _obj##.backgroundColor
    method set_bg_color x = _obj##.backgroundColor := CSS.Color.js x

    (** Title font size *)
    method title_font_size : int = _obj##.titleFontSize
    method set_title_font_size x = _obj##.titleFontSize := x

    (** Title font style *)
    method title_font_style : Font.style = Font.style_of_string_exn @@ Js.to_string _obj##.titleFontStyle
    method set_title_font_style (x:Font.style) = _obj##.titleFontStyle := Js.string @@ Font.style_to_string x

    (** Title font color *)
    method title_font_color : CSS.Color.t = CSS.Color.ml _obj##.titleFontColor
    method set_title_font_color x = _obj##.titleFontColor := CSS.Color.js x

    (** Title font family *)
    method title_font_family : string = Js.to_string _obj##.titleFontFamily
    method set_title_font_family x = _obj##.titleFontFamily := Js.string x

    (** Spacing to add to top and bottom of each title line. *)
    method title_spacing : int = _obj##.titleSpacing
    method set_title_spacing x = _obj##.titleSpacing := x

    (** Margin to add on bottom of title section. *)
    method title_margin_bottom : int = _obj##.titleMarginBottom
    method set_title_margin_bottom x = _obj##.titleMarginBottom := x

    (** Body font size *)
    method body_font_size : int = _obj##.bodyFontSize
    method set_body_font_size x = _obj##.bodyFontSize := x

    (** Body font style *)
    method body_font_style : Font.style   = Font.style_of_string_exn @@ Js.to_string _obj##.bodyFontStyle
    method set_body_font_style (x:Font.style) = _obj##.bodyFontStyle := Js.string @@ Font.style_to_string x

    (** Body font color *)
    method body_font_color : CSS.Color.t = CSS.Color.ml _obj##.bodyFontColor
    method set_body_font_color x = _obj##.bodyFontColor := CSS.Color.js x

    (** Body font family *)
    method body_font_family : string = Js.to_string _obj##.bodyFontFamily
    method set_body_font_family x = _obj##.bodyFontFamily := Js.string x

    (** Spacing to add to top and bottom of each tooltip item. *)
    method body_spacing : int = _obj##.bodySpacing
    method set_body_spacing x = _obj##.bodySpacing := x

    (** Footer font size *)
    method footer_font_size : int = _obj##.footerFontSize
    method set_footer_font_size x = _obj##.footerFontSize := x

    (** Footer font style *)
    method footer_font_style : Font.style =
      Font.style_of_string_exn @@ Js.to_string _obj##.footerFontStyle
    method set_footer_font_style (x:Font.style) =
      _obj##.footerFontStyle := Js.string @@ Font.style_to_string x

    (** Footer font color *)
    method footer_font_color : CSS.Color.t = CSS.Color.ml _obj##.footerFontColor
    method set_footer_font_color x = _obj##.footerFontColor := CSS.Color.js x

    (** Footer font family *)
    method footer_font_family : string = Js.to_string _obj##.footerFontFamily
    method set_footer_font_family x = _obj##.footerFontFamily := Js.string x

    (** Spacing to add to top and bottom of each footer line. *)
    method footer_spacing : int = _obj##.footerSpacing
    method set_footer_spacing x = _obj##.footerSpacing := x

    (** Margin to add before drawing the footer. *)
    method footer_margin_top : int = _obj##.footerMarginTop
    method set_footer_margin_top x = _obj##.footerMarginTop := x

    (** Padding to add on left and right of tooltip. *)
    method x_padding : int = _obj##.xPadding
    method set_x_padding x = _obj##.xPadding := x

    (** Padding to add on top and bottom of tooltip. *)
    method y_padding : int = _obj##.yPadding
    method set_y_padding x = _obj##.yPadding := x

    (** Extra distance to move the end of the tooltip arrow away from the tooltip point. *)
    method caret_padding : int = _obj##.caretPadding
    method set_caret_padding x = _obj##.caretPadding := x

    (** Size, in px, of the tooltip arrow. *)
    method caret_size : int = _obj##.caretSize
    method set_caret_size x = _obj##.caretSize := x

    (** Radius of tooltip corner curves. *)
    method corner_radius : int = _obj##.cornerRadius
    method set_corner_radius x = _obj##.cornerRadius := x

    (** Color to draw behind the colored boxes when multiple items are in the tooltip *)
    method multi_key_background : CSS.Color.t = CSS.Color.ml _obj##.multiKeyBackground
    method set_multi_key_background x = _obj##.multiKeyBackground := CSS.Color.js x

    (** if true, color boxes are shown in the tooltip *)
    method display_colors : bool = Js.to_bool _obj##.displayColors
    method set_display_colors x = _obj##.displayColors := Js.bool x

    (** Color of the border *)
    method border_color : CSS.Color.t = CSS.Color.ml _obj##.borderColor
    method set_border_color x = _obj##.borderColor := CSS.Color.js x

    (** Size of the border *)
    method border_width : int = _obj##.borderWidth
    method set_border_width x = _obj##.borderWidth := x

    initializer
      self#set_enabled true;
      self#set_mode `Nearest;
      self#set_intersect true;
      self#set_position `Average;
      self#set_bg_color @@ CSS.Color.rgb ~a:0.8 0 0 0;
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
