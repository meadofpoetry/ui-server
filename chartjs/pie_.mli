open Types

module Dataset : sig
  type t

  (** The fill color of the arcs in the dataset. *)
  val background_color : t -> Color.t list
  val set_background_color : t -> Color.t list -> unit

  (** The border color of the arcs in the dataset. *)
  val border_color : t -> Color.t list
  val set_border_color : t -> Color.t list -> unit

  (** The border width of the arcs in the dataset. *)
  val border_width : t -> int list
  val set_border_width : t -> int list -> unit

  (** The fill colour of the arcs when hovered. *)
  val hover_background_color : t -> Color.t list
  val set_hover_background_color : t -> Color.t list -> unit

  (** The stroke colour of the arcs when hovered. *)
  val hover_border_color : t -> Color.t list
  val set_hover_border_color : t -> Color.t list -> unit

  (** The stroke width of the arcs when hovered. *)
  val hover_border_width : t -> int list
  val set_hover_border_width : t -> int list -> unit

  (** For a pie chart, datasets need to contain an array of data points.
      The data points should be a number, Chart.js will total all of the
      numbers and calculate the relative proportion of each. *)
  val data : t -> float list
  val set_data : t -> float list -> unit

  val make : ?background_color:Color.t list ->
             ?border_color:Color.t list ->
             ?border_width:int list ->
             ?hover_background_color:Color.t list ->
             ?hover_border_color:Color.t list ->
             ?hover_border_width:int list ->
             ?data:float list ->
             unit ->
             t [@@js.builder]

end

module Data : sig
  type t =
    { mutable datasets : Dataset.t list
    ; mutable labels : string list
    }
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

  val datasets : t -> Dataset.t list
  val set_datasets : t -> Dataset.t list -> unit

  val labels : t -> string list
  val set_labels : t -> string list -> unit

  val make : ?datasets:Dataset.t list ->
             ?labels:string list ->
             unit ->
             t [@@js.builder]

  val cast : t -> Config.Data.t [@@js.cast]

end

module Options : sig

  module Animation : sig
    type t = Animation.t

    (** If true, the chart will animate in with a rotation animation.
        This property is in the options.animation object.*)
    val animate_rotate : t -> bool
    val set_animate_rotate : t -> bool -> unit

    (** If true, will animate scaling the chart from the center outwards. *)
    val animate_scale : t -> bool
    val set_animate_scale : t -> bool -> unit

    val make : ?duration:int ->
               ?easing:easing ->
               ?on_progress:Animation.callback ->
               ?on_complete:Animation.callback ->
               ?animate_rotate:bool ->
               ?animate_scale:bool ->
               unit ->
               t [@@js.builder]

  end

  type t = Options.t

  (** The percentage of the chart that is cut out of the middle. *)
  val cutout_percentage : t -> float
  val set_cutout_percentage : t -> float -> unit

  (** Starting angle to draw arcs from. *)
  val rotation : t -> float
  val set_rotation : t -> float -> unit

  (** Sweep to allow arcs to cover. *)
  val circumference : t -> float
  val set_circumference : t -> float -> unit

end

module Config : sig

  type t =
    { mutable data : Data.t
    ; mutable options : Options.t
    ; type_ : string
    }
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

  val make : ?options:Options.t ->
             data:Data.t ->
             type_:string ->
             unit ->
             t [@@js.builder]

end
