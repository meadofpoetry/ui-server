open Types
open Config

[@@@js.stop]
type canvas = Dom_html.canvasElement Js.t
[@@@js.start]
[@@@js.implem
 type canvas = Dom_html.canvasElement Js.t
]
val canvas_to_js : canvas -> Ojs.t
  [@@js.custom let canvas_to_js (c : canvas) : Ojs.t =
     Obj.magic c
  ]
val canvas_of_js : Ojs.t -> canvas
  [@@js.custom let canvas_of_js (js : Ojs.t) : canvas =
     Obj.magic js
  ]

[@@@js.stop]
type context = Dom_html.canvasRenderingContext2D Js.t
[@@@js.start]
[@@@js.implem
 type context = Dom_html.canvasRenderingContext2D Js.t
]
val context_to_js : context -> Ojs.t
  [@@js.custom
   let context_to_js (c : context) : Ojs.t =
     Obj.magic c
  ]
val context_of_js : Ojs.t -> context
  [@@js.custom
   let context_of_js (js : Ojs.t) : context =
     Obj.magic js
  ]

type node =
  [ `Canvas of canvas
  | `Context of context
  | `Id of string
  ] [@js.union]

(** A config object can be provided with additional configuration
    for the update process. This is useful when update is manually
    called inside an event handler and some different animation is
    desired. *)
type api_config
val make_api_config :
  (** Time for the animation of the redraw in milliseconds. *)
  ?duration:int ->
  (** If true, the animation can be interrupted by other animations. *)
  ?lazy_:bool ->
  (** The animation easing function. *)
  ?easing:easing ->
  unit ->
  api_config [@@js.builder]

type call = ?config:api_config -> unit -> unit

(** TODO add
    'get_element_at_event',
    'get_elements_at_event',
    'get_dataset_meta' *)
type t =
  { id : int
  ; inner_radius : int
  ; height : int
  ; width : int
  ; offset_x : int
  ; offset_y : int
  ; border_width : float
  ; animating : bool
  ; aspect_ratio : float
  ; canvas : canvas
  ; ctx : context
  ; options : Options.options
  (* Chartjs API *)
  ; destroy : unit -> unit
  (** Triggers an update of the chart.
      This can be safely called after updating the data object.
      This will update all scales, legends, and then re-render the chart. *)
  ; update : api_config option -> unit
  (** Reset the chart to it's state before the initial animation.
      A new animation can then be triggered using update. *)
  ; reset : unit -> unit
  (** Triggers a redraw of all chart elements.
      Note, this does not update elements for new data.
      Use .update() in that case. *)
  ; render : api_config option -> unit
  (** Use this to stop any current animation loop.
      This will pause the chart during any current animation frame.
      Call .render() to re-animate. *)
  ; stop : unit -> t
  (** Use this to manually resize the canvas element.
      This is run each time the canvas container is resized,
      but you can call this method manually if you change the size of
      the canvas nodes container element. *)
  ; resize : unit -> t
  (** Will clear the chart canvas. Used extensively internally between
      animation frames, but you might find it useful. *)
  ; clear : unit -> t
  (** This returns a base 64 encoded string of the chart
      in it's current state. *)
  ; to_base64_image : unit -> string
  (** Returns an HTML string of a legend for that chart.
      The legend is generated from the legendCallback in the options. *)
  ; generate_legend : unit -> string
  }
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

val new_chart : node -> Config.t -> t [@@js.new "Chart"]
