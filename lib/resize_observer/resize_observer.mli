open Js_of_ocaml

(** ResizeObserver API

    A code example:

    {[
      if ResizeObserver.is_supported () then
        let doc = Dom_html.document in
        let target =
          Js.Opt.get
            (doc##getElementById (Js.string "observed"))
            (fun () -> assert false)
        in
        let node = (target :> Dom.node Js.t) in
        let f entries observer =
          Firebug.console##debug entries;
          Firebug.console##debug observer
        in
        ResizeObserver.observe ~node ~f ~box:(Js.string "content-box") ()
    ]}
    @see <https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserver> for
    API documentation
    @see <https://drafts.csswg.org/resize-observer> for W3C draft spec *)

class type resizeObserverSize =
  object
    method inlineSize : float Js.readonly_prop

    method blockSize : float Js.readonly_prop
  end

class type resizeObserverEntry =
  object
    method target : Dom.node Js.t Js.readonly_prop

    method contentRect : Dom_html.clientRect Js.t Js.readonly_prop

    method borderBoxSize : resizeObserverSize Js.t Js.readonly_prop

    method contentBoxSize : resizeObserverSize Js.t Js.readonly_prop
  end

class type resizeObserverOptions =
  object
    method box : Js.js_string Js.t Js.prop
  end

class type resizeObserver =
  object
    method observe : #Dom.node Js.t -> unit Js.meth

    method observe_withOptions :
      #Dom.node Js.t -> resizeObserverOptions Js.t -> unit Js.meth

    method unobserve : #Dom.node Js.t -> unit Js.meth

    method disconnect : unit Js.meth
  end

val empty_resize_observer_options : unit -> resizeObserverOptions Js.t

val resizeObserver :
  ((resizeObserverEntry Js.t Js.js_array Js.t -> resizeObserver Js.t -> unit)
   Js.callback ->
  resizeObserver Js.t)
  Js.constr

val is_supported : unit -> bool

val observe :
  node:#Dom.node Js.t ->
  f:(resizeObserverEntry Js.t Js.js_array Js.t -> resizeObserver Js.t -> unit) ->
  ?box:Js.js_string Js.t ->
  unit ->
  resizeObserver Js.t
(** Helper to create a new observer and connect it to a node. *)
