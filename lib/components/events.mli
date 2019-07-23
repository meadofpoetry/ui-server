open Js_of_ocaml

module Typ : sig

  type delta_mode =
    | DOM_DELTA_PIXEL
    | DOM_DELTA_LINE
    | DOM_DELTA_PAGE

  class type wheelEvent =
    object
      inherit Dom_html.mouseEvent
      method deltaX : int Js.readonly_prop
      method deltaY : int Js.readonly_prop
      method deltaZ : int Js.readonly_prop
      method deltaMode : delta_mode Js.readonly_prop
    end

  (** https://www.w3.org/TR/pointerevents/#dom-pointerevent *)
  class type pointerEvent =
    object
      inherit Dom_html.mouseEvent
      method pointerId : int Js.readonly_prop
      method width : float Js.readonly_prop
      method height : float Js.readonly_prop
      method pressure : float Js.readonly_prop
      method tangentialPressure : float Js.readonly_prop
      method tiltX : int Js.readonly_prop
      method tiltY : int Js.readonly_prop
      method twist : int Js.readonly_prop
      method pointerType : Js.js_string Js.t Js.readonly_prop
      method isPrimary : bool Js.t Js.readonly_prop
    end

  val wheel : wheelEvent Js_of_ocaml.Js.t Js_of_ocaml.Dom_events.Typ.typ

  val pointerdown :
    pointerEvent Js_of_ocaml.Js.t Js_of_ocaml.Dom_events.Typ.typ

  val focusin :
    Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t
      Js_of_ocaml.Dom_events.Typ.typ

  val focusout :
    Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t
    Js_of_ocaml.Dom_events.Typ.typ
end

val make_event :
  (#Dom_html.event as 'a) Js.t Dom_html.Event.typ
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> 'a Js.t Lwt.t

val seq_loop :
  (?use_capture:bool -> ?passive:bool -> 'target -> 'event Lwt.t)
  -> ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> 'target
  -> ('event -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val click :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.mouseEvent Js.t Lwt.t

val dblclick :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.mouseEvent Js.t Lwt.t

val mousedown :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.mouseEvent Js.t Lwt.t

val mouseup :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.mouseEvent Js.t Lwt.t

val mouseover :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.mouseEvent Js.t Lwt.t

val mousemove :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.mouseEvent Js.t Lwt.t

val mouseout :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.mouseEvent Js.t Lwt.t

val keypress :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.keyboardEvent Js.t Lwt.t

val keydown :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.keyboardEvent Js.t Lwt.t

val keyup :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.keyboardEvent Js.t Lwt.t

val input :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val timeupdate :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val change :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val dragstart :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.dragEvent Js.t Lwt.t

val dragend :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.dragEvent Js.t Lwt.t

val dragenter :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.dragEvent Js.t Lwt.t

val dragover :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.dragEvent Js.t Lwt.t

val dragleave :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.dragEvent Js.t Lwt.t

val drag :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.dragEvent Js.t Lwt.t

val drop :
  ?use_capture:bool
  -> ?passive:bool -> #Dom_html.eventTarget Js.t -> Dom_html.dragEvent Js.t Lwt.t

val focus :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val blur :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val scroll :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val submit :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val select :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val touchstart :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.touchEvent Js.t Lwt.t

val touchmove :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.touchEvent Js.t Lwt.t

val touchend :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.touchEvent Js.t Lwt.t

val touchcancel :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.touchEvent Js.t Lwt.t

val transitionend : #Dom_html.eventTarget Js.t -> unit Lwt.t
(** Returns when a CSS transition terminates on the element. *)

val transitionends :
  ?cancel_handler:bool
  -> #Dom_html.eventTarget Js.t
  -> (unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val load :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.imageElement Js.t
  -> Dom_html.event Js.t Lwt.t

val error :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.imageElement Js.t
  -> Dom_html.event Js.t Lwt.t

val abort : ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.imageElement Js.t
  -> Dom_html.event Js.t Lwt.t

val canplay :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val canplaythrough :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val durationchange :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t -> Dom_html.event Js.t Lwt.t

val emptied :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val ended :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val loadeddata :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val loadedmetadata :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val loadstart :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val pause :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val play :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val playing :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val ratechange :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val seeked :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val seeking :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val stalled :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val suspend :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val volumechange :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val waiting :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val clicks :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val dblclicks :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val mousedowns :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val mouseups :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val mouseovers :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val mousemoves :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val mouseouts :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val keypresses :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.keyboardEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val keydowns :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.keyboardEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val keyups :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.keyboardEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val inputs :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val timeupdates :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val changes :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val dragstarts :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val dragends :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val dragenters :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val dragovers :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val dragleaves :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val drags :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val drops :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val touchstarts :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val touchmoves :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val touchends :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val touchcancels :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val focuses :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val blurs :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val scrolls :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val submits :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val selects :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val loads :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.imageElement Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val errors :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.imageElement Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val aborts :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.imageElement Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val canplays :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val canplaythroughs :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val durationchanges :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val emptieds :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val endeds :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val loadeddatas :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val loadedmetadatas :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val loadstarts :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val pauses :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val plays :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val playings :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val ratechanges :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val seekeds :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val seekings :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val stalleds :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val suspends :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val volumechanges :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val waitings :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val request_animation_frame : unit -> unit Lwt.t
(** Returns when a repaint of the window by the browser starts.
    (see JS method [window.requestAnimationFrame]) *)

val onload : unit -> Dom_html.event Js.t Lwt.t
(** Returns when the page is loaded *)

val domContentLoaded : unit -> unit Lwt.t

val onunload : unit -> Dom_html.event Js.t Lwt.t

val onbeforeunload : unit -> Dom_html.event Js.t Lwt.t

val onresize : unit -> Dom_html.event Js.t Lwt.t

val onorientationchange : unit -> Dom_html.event Js.t Lwt.t

val onpopstate : unit -> Dom_html.popStateEvent Js.t Lwt.t

val onhashchange : unit -> Dom_html.hashChangeEvent Js.t Lwt.t

val onorientationchange_or_onresize : unit -> Dom_html.event Js.t Lwt.t

val onresizes : (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val onorientationchanges :
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val onpopstates : (Dom_html.popStateEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val onhashchanges :
  (Dom_html.hashChangeEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val onorientationchanges_or_onresizes :
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val limited_onresizes :
  ?elapsed_time:float -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val limited_onorientationchanges :
  ?elapsed_time:float -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val limited_onorientationchanges_or_onresizes :
  ?elapsed_time:float -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

(* XXX not from original JSOO module *)

val wheel :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Typ.wheelEvent Js.t Lwt.t

val wheels :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Typ.wheelEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val pointerdown :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Typ.pointerEvent Js.t Lwt.t

val pointerdowns :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Typ.pointerEvent Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val focusin :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val focusins :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t

val focusout :
  ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> Dom_html.event Js.t Lwt.t

val focusouts :
  ?cancel_handler:bool
  -> ?use_capture:bool
  -> ?passive:bool
  -> #Dom_html.eventTarget Js.t
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t
