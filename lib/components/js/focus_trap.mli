(** @see < https://github.com/davidtheclark/focus-trap >.
    [focus-trap] JavaScript library rewritten in OCaml. *)

(** When a focus trap is activated, this is what should happen:
    - Some element within the focus trap receives focus. By default, this
      will be the first element in the focus trap's tab order. Alternately,
      you can specify an element that should receive this initial focus.
    - The [Tab] and [Shift+Tab] keys will cycle through the focus trap's
      tabbable elements but will not leave the focus trap.
    - Clicks within the focus trap behave normally; but clicks outside the
      focus trap are blocked.
    - The [Escape] key will deactivate the focus trap.
    When the focus trap is deactivated, this is what should happen:
    - Focus is passed to whichever element had focus when the trap was activated
      (e.g. the button that opened the modal or menu).
    - Tabbing and clicking behave normally everywhere. *)

open Js_of_ocaml

type t

(** Activates the focus trap, adding various event listeners to the document. *)
val activate : t -> unit

(** Deactivates the focus trap. *)
val deactivate : ?return_focus:bool -> t -> unit

(** Pause an active focus trap's event listening without deactivating the trap. *)
val pause : t -> unit

(** Unpause an active focus trap. *)
val unpause : t -> unit

(** Returns a new focus trap on an element. *)
val make :
  ?on_activate:(unit -> unit) ->
  ?on_deactivate:(unit -> unit) ->
  ?initial_focus:Dom_html.element Js.t ->
  ?fallback_focus:Dom_html.element Js.t ->
  ?escape_deactivates:bool ->
  ?click_outside_deactivates:bool ->
  ?return_focus_on_deactivate:bool ->
  #Dom_html.element Js.t -> t
