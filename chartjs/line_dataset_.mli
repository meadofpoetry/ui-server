open Types

module Dataset : sig
  type t

  type cubic_interpolation_mode =
    [ `Default [@js "default"]
    | `Monotone [@js "monotone"]
    ] [@js.enum]

  type stepped_line =
    [ `Bool of bool
    | `String of string
    ] [@js.union]

  type int_point_prop =
    [ `Int of int
    | `List of int list
    ] [@js.union]

  val int_point_prop_of_js : Ojs.t -> int_point_prop
    [@@js.custom
     let int_point_prop_of_js (js : Ojs.t) : int_point_prop =
       match Ojs.obj_type js with
       | "[object Number]" ->
          `Int (Ojs.int_of_js js)
       | "[object Array]" ->
          `List (Ojs.array_of_js Ojs.int_of_js js
                 |> Array.to_list)
       | _ -> assert false
    ]

  type color_point_prop =
    [ `Single of Color.t
    | `List of Color.t list
    ] [@js.union]

  val color_point_prop_of_js : Ojs.t -> color_point_prop
    [@@js.custom
     let color_point_prop_of_js (js : Ojs.t) : color_point_prop =
       match Ojs.obj_type js with
       | "[object Array]" ->
          `List (Ojs.array_of_js Ojs.string_of_js js
                 |> Array.to_list)
       | _ -> `Single (Ojs.string_of_js js)
    ]

  type point =
    { x : float
    ; y : float
    }

  (** The label for the dataset which appears in the legend and tooltips. *)
  val label : t -> string
  val set_label : t -> string -> unit

  (** The ID of the x axis to plot this dataset on.
      If not specified, this defaults to the ID of the first found x axis *)
  val x_axis_id : t -> string
  val set_x_axis_id : t -> string -> unit

  (** The ID of the y axis to plot this dataset on.
      If not specified, this defaults to the ID of the first found y axis. *)
  val y_axis_id : t -> string
  val set_y_axis_id : t -> string -> unit

  (** The fill color under the line. *)
  val background_color : t -> Color.t
  val set_background_color : t -> Color.t -> unit

  (** The color of the line. *)
  val border_color : t -> Color.t
  val set_border_color : t -> Color.t -> unit

  (** The width of the line in pixels. *)
  val border_width : t -> int
  val set_border_width : t -> int -> unit

  (** Length and spacing of dashes. *)
  val border_dash : t -> border_dash
  val set_border_dash : t -> border_dash -> unit

  (** Offset for line dashes. *)
  val border_dash_offset : t -> border_dash_offset
  val set_border_dash_offset : t -> border_dash_offset -> unit

  (** Cap style of the line. *)
  val border_cap_style : t -> line_cap
  val set_border_cap_style : t -> line_cap -> unit

  (** Line joint style. *)
  val border_join_style : t -> line_join
  val set_border_join_style : t -> line_join -> unit

  (** Algorithm used to interpolate a smooth
      curve from the discrete data points. *)
  val cubic_interpolation_mode : t -> cubic_interpolation_mode
  val set_cubic_interpolation_mode : t -> cubic_interpolation_mode -> unit

  (** Bezier curve tension of the line. Set to 0 to draw straightlines.
      This option is ignored if monotone cubic interpolation is used. *)
  val line_tension : t -> float
  val set_line_tension : t -> float -> unit

  (** The fill color for points. *)
  val point_background_color : t -> color_point_prop
  val set_point_background_color : t -> color_point_prop -> unit

  (** The border color for points. *)
  val point_border_color : t -> color_point_prop
  val set_point_border_color : t -> color_point_prop -> unit

  (** The width of the point border in pixels. *)
  val point_border_width : t -> int_point_prop
  val set_point_border_width : t -> int_point_prop -> unit

  (** The radius of the point shape. If set to 0, the point is not rendered. *)
  val point_radius : t -> int_point_prop
  val set_point_radius : t -> int_point_prop -> unit

  (** The rotation of the point in degrees. *)
  val point_rotation : t -> int_point_prop
  val set_point_rotation : t -> int_point_prop -> unit

  (** The pixel size of the non-displayed point that reacts to mouse events. *)
  val point_hit_radius : t -> int_point_prop
  val set_point_hit_radius : t -> int_point_prop -> unit

  val point_hover_background_color : t -> color_point_prop
  val set_point_hover_background_color : t -> color_point_prop -> unit

  val point_hover_border_color : t -> color_point_prop
  val set_point_hover_border_color : t -> color_point_prop -> unit

  (** Border width of point when hovered. *)
  val point_hover_border_width : t -> int_point_prop
  val set_point_hover_border_width : t -> int_point_prop -> unit

  (** The radius of the point when hovered. *)
  val point_hover_radius : t -> int_point_prop
  val set_point_hover_radius : t -> int_point_prop -> unit

  (** If false, the line is not drawn for this dataset. *)
  val show_line : t -> bool
  val set_show_line : t -> bool -> unit

  (** If true, lines will be drawn between points with no or null data.
      If false, points with NaN data will create a break in the line *)
  val span_gaps : t -> bool
  val set_span_gaps : t -> bool -> unit

  (** If the line is shown as a stepped line. *)
  (* TODO val stepped_line : t -> ([`Bool of bool | `String of string] [@js.sum]) *)
  val set_stepped_line : t -> stepped_line -> unit

  val data : t -> point list
  val set_data : t -> point list -> unit

  val make : ?label:string ->
             ?x_axis_id:string ->
             ?y_axis_id:string ->
             ?background_color:Color.t ->
             ?border_color:Color.t ->
             ?border_width:int ->
             ?border_dash:border_dash ->
             ?border_dash_offset:border_dash_offset ->
             ?border_cap_style:line_cap ->
             ?border_join_style:line_join ->
             ?cubic_interpolation_mode:cubic_interpolation_mode ->
             ?line_tension:float ->
             ?point_radius:int_point_prop ->
             ?show_line:bool ->
             ?span_gaps:bool ->
             ?stepped_line:stepped_line ->
             ?data:(point list) ->
             unit ->
             t [@@js.builder]

end

module Data : sig
  type t

  val datasets : t -> Dataset.t list

  val set_datasets : t -> Dataset.t list -> unit

  val make : datasets:(Dataset.t list) -> t [@@js.builder]

end

module Config : sig
  type t = private Ojs.t

  val make : ?data:Data.t ->
             ?options:Options_.t ->
             (string[@js "type"]) ->
             t [@@js.builder]

end

module Chart : sig
  type t

  val new_chart : Ojs.t -> Config.t -> t [@@js.new "Chart"]

end
