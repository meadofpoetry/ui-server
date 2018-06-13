open Base

type color_or_color_array
type int_or_int_array

type clr = [`Single of CSS.Color.t | `Several of CSS.Color.t list]
type lw  = [`Single of int | `Several of int list]

class type t_js =
  object
    method display                  : bool Js.t Js.prop
    method color                    : color_or_color_array Js.t Js.prop
    method borderDash               : int Js.js_array Js.t Js.prop
    method borderDashOffset         : int Js.prop
    method lineWidth                : int_or_int_array Js.t Js.prop
    method drawBorder               : bool Js.t Js.prop
    method drawOnChartArea          : bool Js.t Js.prop
    method drawTicks                : bool Js.t Js.prop
    method tickMarkLength           : int Js.prop
    method zeroLineWidth            : int Js.prop
    method zeroLineColor            : CSS.Color.js_t Js.prop
    method zeroLineBorderDash       : int Js.js_array Js.t Js.prop
    method zeroLineBorderDashOffset : int Js.prop
    method offsetGridLines          : bool Js.t Js.prop
  end

(** The grid line configuration defines options for the grid lines
    that run perpendicular to the axis *)
class t () = object(self)

  inherit [t_js] base_option ()

  (** If false, do not display grid lines for this axis. *)
  method display : bool = Js.to_bool obj##.display
  method set_display x = obj##.display := Js.bool x

  (** The color of the grid lines. If specified as an array, the first color applies
      to the first grid line, the second to the second grid line and so on. *)
  method color : clr = match Cast.to_list ~f:CSS.Color.ml obj##.color with
    | Some l -> `Several l
    | None   -> `Single (CSS.Color.ml @@ CSS.Color.js_t_of_js_string @@ Js.Unsafe.coerce obj##.color)
  method set_color : clr -> unit = function
    | `Single c  -> let v = Js.string @@ CSS.Color.string_of_t c in
                    obj##.color := Js.Unsafe.coerce v
    | `Several l -> let v = List.map CSS.Color.js l |> Array.of_list |> Js.array in
                    obj##.color := Js.Unsafe.coerce v

  (** Length and spacing of dashes on grid lines. *)
  method border_dash : int list = Array.to_list @@ Js.to_array obj##.borderDash
  method set_border_dash x = obj##.borderDash := Js.array @@ Array.of_list x

  (** Offset for line dashes. *)
  method border_dash_offset : int = obj##.borderDashOffset
  method set_border_dash_offset x = obj##.borderDashOffset := x

  (** Stroke width of grid lines. *)
  method line_width : lw = match Cast.to_list ~f:(fun x -> x) obj##.lineWidth with
    | Some l -> `Several l
    | None   -> `Single (int_of_float @@ Js.float_of_number (Js.Unsafe.coerce obj##.lineWidth))
  method set_line_width : lw -> unit = function
    | `Single w  -> obj##.lineWidth := Js.Unsafe.coerce @@ Js.number_of_float @@ float_of_int w
    | `Several l -> obj##.lineWidth := Js.Unsafe.coerce @@ Js.array @@ Array.of_list l

  (** If true, draw border at the edge between the axis and the chart area. *)
  method draw_border : bool = Js.to_bool obj##.drawBorder
  method set_draw_border x = obj##.drawBorder := Js.bool x

  (** If true, draw lines on the chart area inside the axis lines.
      This is useful when there are multiple axes and you need
      to control which grid lines are drawn. *)
  method draw_on_chart_area : bool = Js.to_bool obj##.drawOnChartArea
  method set_draw_on_chart_area x = obj##.drawOnChartArea := Js.bool x

  (** If true, draw lines beside the ticks in the axis area beside the chart. *)
  method draw_ticks : bool = Js.to_bool obj##.drawTicks
  method set_draw_ticks x = obj##.drawTicks := Js.bool x

  (** Length in pixels that the grid lines will draw into the axis area. *)
  method tick_mark_length : int = obj##.tickMarkLength
  method set_tick_mark_length x = obj##.tickMarkLength := x

  (** Stroke width of the grid line for the first index (index 0). *)
  method zero_line_width : int = obj##.zeroLineWidth
  method set_zero_line_width x = obj##.zeroLineWidth := x

  (** Stroke color of the grid line for the first index (index 0). *)
  method zero_line_color : CSS.Color.t = CSS.Color.ml obj##.zeroLineColor
  method set_zero_line_color x = obj##.zeroLineColor := CSS.Color.js x

  (** Length and spacing of dashes of the grid line for the first index (index 0). *)
  method zero_line_border_dash : int list = Array.to_list @@ Js.to_array obj##.zeroLineBorderDash
  method set_zero_line_border_dash x = obj##.zeroLineBorderDash := Js.array @@ Array.of_list x

  (** Offset for line dashes of the grid line for the first index (index 0). *)
  method zero_line_border_dash_offset : int = obj##.zeroLineBorderDashOffset
  method set_zero_line_border_dash_offset x = obj##.zeroLineBorderDashOffset := x

  (** If true, grid lines will be shifted to be between labels.
      This is set to true in the bar chart by default. *)
  method offset_grid_lines : bool = Js.to_bool obj##.offsetGridLines
  method set_offset_grid_lines x = obj##.offsetGridLines := Js.bool x

  initializer
    self#set_display true;
    self#set_color @@ `Single (CSS.Color.rgb ~a:0.1 0 0 0);
    self#set_border_dash [];
    self#set_border_dash_offset 0;
    self#set_line_width @@ `Single 1;
    self#set_draw_border true;
    self#set_draw_on_chart_area true;
    self#set_draw_ticks true;
    self#set_tick_mark_length 10;
    self#set_zero_line_width 1;
    self#set_zero_line_color @@ CSS.Color.rgb ~a:0.25 0 0 0;
    self#set_zero_line_border_dash [];
    self#set_zero_line_border_dash_offset 0;
    self#set_offset_grid_lines false
end
