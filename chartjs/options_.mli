open Types

module Elements : sig

  module Point : sig
    type t

    type style =
      [ `Circle [@js "circle"]
      | `Cross [@js "cross"]
      | `Cross_rot [@js "crossRot"]
      | `Dash [@js "dash"]
      | `Line [@js "line"]
      | `Rect [@js "rect"]
      | `Rect_rounded [@js "rectRounded"]
      | `Rect_rot [@js "rectRot"]
      | `Star [@js "star"]
      | `Triangle [@js "triangle"]
      ] [@js.enum]

    (** Point radius. *)
    val radius : t -> int
    val set_radius : t -> int -> unit

    (** Point style. *)
    val point_style : t -> style
    val set_point_style : t -> style -> unit

    (** Point rotation (in degrees). *)
    val rotation : t -> int
    val set_rotation : t -> int -> unit

    (** Point fill color. *)
    val background_color : t -> Color.t
    val set_background_color : t -> Color.t -> unit

    (** Point stroke width. *)
    val border_width : t -> int
    val set_border_width : t -> int -> unit

    (** Point stroke color. *)
    val border_color : t -> Color.t
    val set_border_color : t -> Color.t -> unit

    (** Extra radius added to point radius for hit detection. *)
    val hit_radius : t -> int
    val set_hit_radius : t -> int -> unit

    (** Point radius when hovered. *)
    val hover_radius : t -> int
    val set_hover_radius : t -> int -> unit

    (** Stroke width when hovered. *)
    val hover_border_width : t -> int
    val set_hover_border_width : t -> int -> unit

    val make : ?radius:int ->
               ?point_style:style ->
               ?rotation:int ->
               ?background_color:Color.t ->
               ?border_width:int ->
               ?border_color:Color.t ->
               ?hit_radius:int ->
               ?hover_radius:int ->
               ?hover_border_width:int ->
               unit ->
               t [@@js.builder]

  end

  module Line : sig
    type t

    type fill =
      [ `Zero
      | `Top
      | `Bottom
      | `Bool of bool (* true -> 'zero', false -> disabled *)
      ] [@js.union]
    val fill_to_js : fill -> Ojs.t
      [@@js.custom
       let fill_to_js : fill -> Ojs.t = function
         | `Zero -> Ojs.string_to_js "zero"
         | `Top -> Ojs.string_to_js "top"
         | `Bottom -> Ojs.string_to_js "bottom"
         | `Bool x -> Ojs.bool_to_js x
      ]
    val fill_of_js : Ojs.t -> fill
      [@@js.custom
       let fill_of_js (js : Ojs.t) : fill =
         match Ojs.obj_type js with
         | "[object Boolean]" -> `Bool (Ojs.bool_of_js js)
         | "[object String]" ->
            begin match Ojs.string_of_js js with
            | "zero" -> `Zero
            | "top" -> `Top
            | "bottom" -> `Bottom
            | _ -> assert false
            end
         | _ -> assert false
      ]

    (** Bézier curve tension (0 for no Bézier curves). *)
    val tension : t -> float
    val set_tension : t -> float -> unit

    (** Line fill color. *)
    val background_color : t -> Color.t
    val set_background_color : t -> Color.t -> unit

    (** Line stroke width. *)
    val border_width : t -> int
    val set_border_width : t -> int -> unit

    (** Line stroke color. *)
    val border_color : t -> Color.t
    val set_border_color : t -> Color.t -> unit

    (** Line cap style. *)
    val border_cap_style : t -> line_cap
    val set_border_cap_style : t -> line_cap -> unit

    (** Line dash. *)
    val border_dash : t -> int list
    val set_border_dash : t -> int list -> unit

    (** Line dash offset. *)
    val border_dash_offset : t -> float
    val set_border_dash_offset : t -> float -> unit

    (** Line join style. *)
    val border_join_style : t -> line_join
    val set_border_join_style : t -> line_join -> unit

    (** 'true' to keep Bézier control inside the chart,
        'false' for no restriction.*)
    val cap_bezier_points : t -> bool
    val set_cap_bezier_points : t -> bool -> unit

    (** Fill location: 'zero', 'top', 'bottom', true (eq. 'zero')
        or false (no fill). *)
    val fill : t -> fill
    val set_fill : t -> fill -> unit

    (** 'true' to show the line as a stepped line
        (tension will be ignored). *)
    val stepped : t -> bool
    val set_stepped : t -> bool -> unit

    val make : ?tension:float ->
               ?background_color:Color.t ->
               ?border_width:int ->
               ?border_color:Color.t ->
               ?border_cap_style:line_cap ->
               ?border_dash:int list ->
               ?border_dash_offset:float ->
               ?border_join_style:line_join ->
               ?cap_bezier_points:bool ->
               ?fill:fill ->
               ?stepped:bool ->
               unit ->
               t [@@js.builder]

  end

  module Rectangle : sig
    type t

    type border_skipped =
      [ `Bottom [@js "bottom"]
      | `Left [@js "left"]
      | `Top [@js "top"]
      | `Right [@js "right"]
      ] [@js.enum]

    (** Bar fill color. *)
    val background_color : t -> Color.t
    val set_background_color : t -> Color.t -> unit

    (** Bar stroke width. *)
    val border_width : t -> int
    val set_border_width : t -> int -> unit

    (** Bar stroke color. *)
    val border_color : t -> Color.t
    val set_border_color : t -> Color.t -> unit

    (** Skipped (excluded) border: 'bottom', 'left', 'top' or 'right'. *)
    val border_skipped : t -> border_skipped
    val set_border_skipped : t -> border_skipped -> unit

    val make : ?background_color:Color.t ->
               ?border_width:int ->
               ?border_color:Color.t ->
               ?border_skipped:border_skipped ->
               unit ->
               t [@@js.builder]

  end

  module Arc : sig
    type t

    (** Arc fill color. *)
    val background_color : t -> Color.t
    val set_background_color : t -> Color.t -> unit

    (** Arc stroke color. *)
    val border_color : t -> Color.t
    val set_border_color : t -> Color.t -> unit

    (** Arc stroke width. *)
    val border_width : t -> int
    val set_border_width : t -> int -> unit

  end

  type t

  (** Point elements are used to represent the points
      in a line chart or a bubble chart. *)
  val point : t -> Point.t
  val set_point : t -> Point.t -> unit

  (** Line elements are used to represent the line in a line chart. *)
  val line : t -> Line.t
  val set_line : t -> Line.t -> unit

  (** Rectangle elements are used to represent the bars in a bar chart. *)
  val rectangle : t -> Rectangle.t
  val set_rectangle : t -> Rectangle.t -> unit

  (** Arcs are used in the polar area, doughnut and pie charts. *)
  val arc : t -> Arc.t
  val set_arc : t -> Arc.t -> unit

  val make : ?point:Point.t ->
             ?line:Line.t ->
             ?rectangle:Rectangle.t ->
             ?arc:Arc.t ->
             unit ->
             t [@@js.builder]
end

module Legend : sig

  module Item : sig
    type t

    (** Label that will be displayed. *)
    val text : t -> string option
    val set_text : t -> string -> unit

    (** Fill style of the legend box *)
    val fill_style : t -> Color.t option
    val set_fill_style : t -> Color.t -> unit

    (** If true, this item represents a hidden dataset.
        Label will be rendered with a strike-through effect *)
    val hidden : t -> bool
    val set_hidden : t -> bool -> unit

    (** For box border. *)
    val line_cap : t -> line_cap option
    val set_line_cap : t -> line_cap -> unit

    (** For box border. *)
    val line_dash : t -> int list option
    val set_line_dash : t -> int list -> unit

    (** For box border. *)
    val line_join : t -> line_join option
    val set_line_join : t -> line_join -> unit

    (** Width of box border. *)
    val line_width : t -> int option
    val set_line_width : t -> int -> unit

    (** Stroke style of the legend box *)
    val stroke_style : t -> Color.t option
    val set_stroke_style : t -> Color.t -> unit

    (** Point style of the legend box (only used if usePointStyle is true) *)
    val point_style : t -> Elements.Point.style option
    val set_point_style : t -> Elements.Point.style -> unit

    val dataset_index : t -> int option
    val set_dataset_index : t -> int -> unit

    val make : ?fill_style:Color.t ->
               ?hidden:bool ->
               ?line_cap:line_cap ->
               ?line_dash:int list ->
               ?line_dash_offset:float ->
               ?line_join:line_join ->
               ?line_width:int ->
               ?stroke_style:Color.t ->
               ?point_style:Elements.Point.style ->
               ?dataset_index:int ->
               ?text:string ->
               unit ->
               t [@@js.builder]

  end

  module Labels : sig

    type generate = chart:Ojs.t -> Item.t list

    type filter = item:Item.t -> data:Ojs.t -> bool

    type t

    (** Width of coloured box. *)
    val box_width : t -> int
    val set_box_width : t -> int -> unit

    (** Font size of text. *)
    val font_size : t -> int
    val set_font_size : t -> int -> unit

    (** Font style of text. *)
    val font_style : t -> Font.style
    val set_font_style : t -> Font.style -> unit

    (** Color of text. *)
    val font_color : t -> Color.t
    val set_font_color : t -> Color.t -> unit

    (** Font family of legend text. *)
    val font_family : t -> Font.family
    val set_font_family : t -> Font.family -> unit

    (** Padding between rows of colored boxes. *)
    val padding : t -> int
    val set_padding : t -> int -> unit

    (** Generates legend items for each thing in the legend.
        Default implementation returns the text + styling for the color box. *)
    val generate_labels : t -> generate
    val set_generate_labels : t -> generate -> unit

    (** Filters legend items out of the legend. Receives 2 parameters,
        a Legend Item and the chart data. *)
    val filter : t -> filter
    val set_filter : t -> filter -> unit

    (** Label style will match corresponding point style
        (size is based on fontSize, boxWidth is not used in this case). *)
    val use_point_style : t -> bool
    val set_use_point_style : t -> bool -> unit

    val make : ?box_width:int ->
               ?font_size:int ->
               ?font_style:Font.style ->
               ?font_color:Color.t ->
               ?font_family:Font.family ->
               ?padding:int ->
               ?generate_labels:generate ->
               ?filter:filter ->
               ?use_point_style:bool ->
               unit ->
               t [@@js.builder]

  end

  type position =
    [ `Left [@js "left"]
    | `Right [@js "right"]
    | `Top [@js "top"]
    | `Bottom [@js "bottom"]
    ] [@js.enum]

  type handler = event:Ojs.t -> item:Item.t -> unit

  type t

  (** Is the legend shown. *)
  val display : t -> bool
  val set_display : t -> bool -> unit

  (** Position of the legend. *)
  val position : t -> position
  val set_position : t -> position -> unit

  (** Marks that this box should take the full width of the canvas
      (pushing down other boxes). This is unlikely to need to be changed
      in day-to-day use. *)
  val full_width : t -> bool
  val set_full_width : t -> bool -> unit

  (** A callback that is called when a click event is registered on a label item *)
  val on_click : t -> handler
  val set_on_click : t -> handler -> unit

  (** A callback that is called when a 'mousemove' event is
      registered on top of a label item *)
  val on_hover : t -> handler
  val set_on_hover : t -> handler -> unit

  (** Legend will show datasets in reverse order. *)
  val reverse : t -> bool
  val set_reverse : t -> bool -> unit

  (** Legend label configuration. *)
  val labels : t -> Labels.t
  val set_labels : t -> Labels.t -> unit

  val make : ?display:bool ->
             ?position:position ->
             ?full_width:bool ->
             ?on_click:handler ->
             ?on_hover:handler ->
             ?reverse:bool ->
             ?labels:Labels.t ->
             unit ->
             t [@@js.builder]

end

module Title : sig

  type text = string
  val text_to_js : text -> Ojs.t
    [@@js.custom
     let text_to_js (s : text) : Ojs.t =
       match String.split_on_char '\n' s with
       | [s] -> Ojs.string_to_js s
       | l -> Ojs.list_to_js Ojs.string_to_js l
    ]
  val text_of_js : Ojs.t -> text
    [@@js.custom
     let text_of_js (js : Ojs.t) : text =
       match Ojs.obj_type js with
       | "[object Array]" ->
          let l = Ojs.list_of_js Ojs.string_of_js js in
          String.concat "\n" l
       | "[object String]" ->
          Ojs.string_of_js js
       | _ -> assert false
    ]

  type position =
    [ `Left [@js "left"]
    | `Right [@js "right"]
    | `Top [@js "top"]
    | `Bottom [@js "bottom"]
    ] [@js.enum]

  type t

  (** Is the title shown. *)
  val display : t -> bool
  val set_display : t -> bool -> unit

  (** Position of title. *)
  val position : t -> position
  val set_position : t -> position -> unit

  (** Font size. *)
  val font_size : t -> int
  val set_font_size : t -> int -> unit

  (** Font family for the title text. *)
  val font_family : t -> Font.family
  val set_font_family : t -> Font.family -> unit

  (** Font color. *)
  val font_color : t -> Color.t
  val set_font_color : t -> Color.t -> unit

  (** Font style. *)
  val font_style : t -> Font.style
  val set_font_style : t -> Font.style -> unit

  (** Number of pixels to add above and below the title text. *)
  val padding : t -> int
  val set_padding : t -> int -> unit

  (** Height of an individual line of text. *)
  val line_height : t -> line_height
  val set_line_height : t -> line_height -> unit

  (** Title text to display. If specified as an array,
      text is rendered on multiple lines. *)
  val text : t -> string
  val set_text : t -> string -> unit

  val make : ?display:bool ->
             ?position:position ->
             ?font_size:int ->
             ?font_family:Font.family ->
             ?font_color:Color.t ->
             ?font_style:Font.style ->
             ?padding:int ->
             ?line_height:line_height ->
             ?text:text ->
             unit ->
             t [@@js.builder]

end

(** The configuration is used to change how the chart behaves.
    There are properties to control styling, fonts, the legend, etc.*)
type t
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

(** While chart types provide settings to configure the styling
    of each dataset, you sometimes want to style all datasets the same way.
    A common example would be to stroke all of the bars in a bar chart with
    the same colour but change the fill per dataset.
    Options can be configured for four different types of elements: arc, lines,
    points, and rectangles. When set, these options apply to all objects
    of that type unless specifically overridden by the configuration attached
    to a dataset.*)
val elements : t -> Elements.t
val set_elements : t -> Elements.t -> unit

(** The chart legend displays data about the datasets
    that are appearing on the chart. *)
val legend : t -> Legend.t
val set_legend : t -> Legend.t -> unit

(** The chart title defines text to draw at the top of the chart. *)
val title : t -> Title.t
val set_title : t -> Title.t -> unit

type legend_callback = chart:Ojs.t -> string

val legend_callback : t -> legend_callback
val set_legend_callback : t -> legend_callback -> unit

val make : ?elements:Elements.t ->
           ?legend:Legend.t ->
           ?title:Title.t ->
           ?legend_callback:legend_callback ->
           unit ->
           t [@@js.builder]
