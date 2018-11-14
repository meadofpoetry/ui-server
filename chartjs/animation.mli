open Types

type animation =
  { chart : Ojs.t
  ; current_step : float option
  ; num_steps : float
  ; easing : easing option
  ; render : (chart:Ojs.t -> animation -> unit) option
  ; on_animation_progress : callback option
  ; on_animation_complete : callback option
  }
and callback = animation -> unit

val animation_to_js : animation -> Ojs.t
val animation_of_js : Ojs.t -> animation

val callback_to_js : callback -> Ojs.t
val callback_of_js : Ojs.t -> callback

type t
val t_to_js : t -> Ojs.t
val t_of_js : Ojs.t -> t

(** The number of milliseconds an animation takes. *)
val duration : t -> int
val set_duration : t -> int -> unit

(** Easing function to use. *)
val easing : t -> easing
val set_easing : t -> easing -> unit

(** Callback called on each step of an animation. *)
val on_progress : t -> callback
val set_on_progress : t -> callback -> unit

(** Callback called at the end of an animation. *)
val on_complete : t -> callback
val set_on_complete : t -> callback -> unit

val make : ?duration:int ->
           ?easing:easing ->
           ?on_progress:callback ->
           ?on_complete:callback ->
           unit ->
           t [@@js.builder]
