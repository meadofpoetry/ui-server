open Js_of_ocaml

class type resizeEntry =
  object
    method target : Dom.node Js.t Js.readonly_prop
    method contentRect : Dom_html.clientRect Js.t Js.readonly_prop
  end

class type resizeObserver =
  object
    method observe : 'a. (#Dom.node as 'a) Js.t -> unit Js.meth
    method unobserve : 'a. (#Dom.node as 'a) Js.t -> unit Js.meth
    method disconnect : unit Js.meth
  end

let resizeObserver = Js.Unsafe.global##._ResizeObserver

let is_supported () = Js.Optdef.test resizeObserver

let resizeObserver :
      ((resizeEntry Js.t Js.js_array Js.t -> unit)
         Js.callback -> resizeObserver Js.t) Js.constr =
  resizeObserver

let observe ~(node : #Dom.node Js.t)
      ~(f : resizeEntry Js.t Js.js_array Js.t -> unit)
      () : resizeObserver Js.t =
  let (obs : resizeObserver Js.t) = new%js resizeObserver (Js.wrap_callback f) in
  obs##observe node;
  obs

let disconnect (obs : resizeObserver Js.t) : unit =
  obs##disconnect
