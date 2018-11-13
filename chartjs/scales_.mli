open Types

module Scale_label : sig
  type padding =
    [ `Obj of padding_obj
    | `Int of int
    ] [@js.union]
  and padding_obj =
    { top : int option
    ; bottom : int option
    }
  val padding_of_js : Ojs.t -> padding
    [@@js.custom
     let padding_of_js (js : Ojs.t) : padding =
       match Ojs.obj_type with
       | "[object Number]" -> `Val (Ojs.int_of_js js)
       | "[object Object]" ->
          let x =
            { top = Ojs.int_of_js @@ Ojs.get js "top"
            ; bottom = Ojs.int_of_js @@ Ojs.get js "bottom"
            } in
          `Obj x
       | _ -> assert false
    ]
  type t

  (** If true, display the axis title. *)
  val display : t -> bool
  val set_display : t -> bool -> unit

  (** The text for the title. (i.e. "# of People" or "Response Choices"). *)
  val label_string : t -> string
  val set_label_string : t -> string -> unit

  (** Height of an individual line of text. *)
  val line_height : t -> line_height
  val set_line_height : t -> line_height -> unit

  (** Font color for scale title. *)
  val font_color : t -> Color.t
  val set_font_color : t -> Color.t -> unit

  (** Font family for the scale title, follows CSS font-family options. *)
  val font_family : t -> Font.family
  val set_font_family : t -> Font.family -> unit

  (** Font size for scale title. *)
  val font_size : t -> int
  val set_font_size : t -> int -> unit

  (** Font style for the scale title, follows CSS font-style options
      (i.e. normal, italic, oblique, initial, inherit) *)
  val font_style : t -> Font.style
  val set_font_style : t -> Font.style -> unit

  (** Padding to apply around scale labels.
      Only top and bottom are implemented. *)
  val padding : t -> padding
  val set_padding : t -> padding -> unit

end

module Grid_lines : sig
  type color =
    [ `Single of Color.t
    | `List of Color.t list
    ] [@js.union]
  val color_of_js : Ojs.t -> Color.t
    [@@js.custom
     let color_of_js (js : Ojs.t) : color =
       match Ojs.obj_type js with
       | "[object Array]" ->
          `List (Ojs.list_of_js Color.t_of_js js)
       | _ -> `Single (Color.t_of_js js)
    ]
  type line_width =
    [ `Single of int
    | `List of int list
    ] [@js.union]
  val line_width_of_js : Ojs.t -> line_width
    [@@js.custom
     let line_width_of_js (js : Ojs.t) : line_width =
       match Ojs.obj_type js with
       | "[object Array]" ->
          `List (Ojs.list_of_js Ojs.int_of_js js)
       | "[object Number]" ->
          `Single (Ojs.int_of_js js)
       | _ -> assert false
    ]
  type t

  (** If false, do not display grid lines for this axis. *)
  val display : t -> bool
  val set_display : t -> bool -> unit

  (** If true, gridlines are circular (on radar chart only). *)
  val circular : t -> bool
  val set_circular : t -> bool -> unit

  (** The color of the grid lines. If specified as an array,
      the first color applies to the first grid line, the second
      to the second grid line and so on. *)
  val color : t -> color
  val set_color : t -> color -> unit

  (** Length and spacing of dashes on grid lines. *)
  val border_dash : t -> border_dash
  val set_border_dash : t -> border_dash -> unit

  (** Offset for line dashes. *)
  val border_dash_offset : t -> border_dash_offset
  val set_border_dash_offset : t -> border_dash_offset -> unit

  (** Stroke width of grid lines. *)
  val line_width : t -> line_width
  val set_line_width : t -> line_width -> unit

  (** If true, draw border at the edge between the axis and the chart area. *)
  val draw_border : t -> bool
  val set_draw_border : t -> bool -> unit

  (** If true, draw lines on the chart area inside the axis lines.
      This is useful when there are multiple axes and you need to
      control which grid lines are drawn. *)
  val draw_on_chart_area : t -> bool
  val set_draw_on_chart_area : t -> bool -> unit

  (** If true, draw lines beside the ticks in the axis area beside the chart. *)
  val draw_ticks : t -> bool
  val set_draw_ticks : t -> bool -> unit

  (** Length in pixels that the grid lines will draw into the axis area. *)
  val tick_mark_length : t -> int
  val set_tick_mark_length : t -> int -> unit

  (** Stroke width of the grid line for the first index (index 0). *)
  val zero_line_width : t -> int
  val set_zero_line_width : t -> int -> unit

  (** Stroke color of the grid line for the first index (index 0). *)
  val zero_line_color : t -> Color.t
  val set_zero_line_color : t -> Color.t -> unit

  (** Length and spacing of dashes of the grid line
      for the first index (index 0). *)
  val zero_line_border_dash : t -> border_dash
  val set_zero_line_border_dash : t -> border_dash -> unit

  (** Offset for line dashes of the grid line for the first index (index 0). *)
  val zero_line_border_dash_offset : t -> border_dash_offset
  val set_zero_line_border_dash_offset : t -> border_dash_offset -> unit

  (** If true, grid lines will be shifted to be between labels.
      This is set to true for a category scale in a bar chart by default. *)
  val offset_grid_lines : t -> bool
  val set_offset_grid_lines : t -> bool -> unit

  val make : ?display:bool ->
             ?circular:bool ->
             ?color:color ->
             ?border_dash:border_dash ->
             ?border_dash_offset:border_dash_offset ->
             ?line_width:line_width ->
             ?draw_border:bool ->
             ?draw_on_chart_area:bool ->
             ?draw_ticks:bool ->
             ?tick_mark_length:int ->
             ?zero_line_width:int ->
             ?zero_line_color:Color.t ->
             ?zero_line_border_dash:border_dash ->
             ?zero_line_border_dash_offset:border_dash_offset ->
             ?offset_grid_lines:bool ->
             unit ->
             t [@@js.builder]

end
