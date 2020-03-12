open Js_of_ocaml

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

let empty_resize_observer_options () : resizeObserverOptions Js.t =
  Js.Unsafe.obj [||]

let resizeObserver = Js.Unsafe.global##._ResizeObserver

let is_supported () = Js.Optdef.test resizeObserver

let resizeObserver :
    ((resizeObserverEntry Js.t Js.js_array Js.t -> resizeObserver Js.t -> unit)
     Js.callback ->
    resizeObserver Js.t)
    Js.constr =
  resizeObserver

let observe ~(node : #Dom.node Js.t)
    ~(f :
       resizeObserverEntry Js.t Js.js_array Js.t -> resizeObserver Js.t -> unit)
    ?(box : Js.js_string Js.t option) () : resizeObserver Js.t =
  let obs = new%js resizeObserver (Js.wrap_callback f) in
  ( match box with
  | None -> obs##observe node
  | Some box ->
      let opts = empty_resize_observer_options () in
      opts##.box := box;
      obs##observe_withOptions node opts );
  obs
