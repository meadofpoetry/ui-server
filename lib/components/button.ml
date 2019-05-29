open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

include Components_tyxml.Button
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let ( >>= ) = Lwt.bind

class t ?(ripple = true) ?on_click ?loader
    (elt : Dom_html.buttonElement Js.t) () =
  object(self)
    val mutable _loader = Option.map Widget.coerce loader
    val mutable _loader_container : Dom_html.element Js.t option =
      Element.query_selector elt CSS.loader_container
    val mutable _ripple : Ripple.t option = None
    val mutable _click_listener : unit Lwt.t option = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      if ripple then
        let ripple = Ripple.attach super#root in
        _ripple <- Some ripple

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      match on_click with
      | None -> ()
      | Some f ->
        let listener = Events.clicks super#root (f (self :> t)) in
        _click_listener <- Some listener

    method! layout () : unit =
      super#layout ();
      (* Layout internal components. *)
      Option.iter Ripple.layout _ripple;
      Option.iter Widget.layout _loader

    method! destroy () : unit =
      super#destroy ();
      (* Destroy internal components. *)
      Option.iter Ripple.destroy _ripple;
      _ripple <- None;
      Option.iter Widget.destroy _loader;
      _loader <- None

    method disabled : bool =
      Js.to_bool elt##.disabled

    method set_disabled (x : bool) : unit =
      elt##.disabled := Js.bool x

    method loading : bool =
      super#has_class CSS.loading

    method set_loading_lwt : 'a. 'a Lwt.t -> unit = fun t ->
      self#set_loading true;
      Lwt.on_termination t (fun () -> self#set_loading false)

    method set_loading (x : bool) : unit =
      if x then (
        super#add_class CSS.loading;
        self#set_disabled true;
        let loader_container =
          match _loader_container with
          | Some x -> x
          | None ->
            let (loader : Widget.t) =
              match _loader with
              | None ->
                let progress = (Circular_progress.make ~size:25 ())#widget in
                _loader <- Some progress;
                progress
              | Some x -> x in
            let container =
              Tyxml_js.To_dom.of_element
              @@ Markup.create_loader_container (Widget.to_markup loader) () in
            _loader_container <- Some container;
            container in
        Element.append_child super#root loader_container)
      else (
        super#remove_class CSS.loading;
        self#set_disabled false;
        Option.iter (Element.remove_child_safe super#root) _loader_container)
  end

let make ?typ ?appearance ?icon ?dense ?ripple ?label ?loader ?on_click () : t =
  let icon = match icon with
    | None -> None
    | Some (i : #Widget.t) ->
      i#add_class CSS.icon;
      Some (Widget.to_markup i) in
  let (elt : Dom_html.buttonElement Js.t) =
    Tyxml_js.To_dom.of_button
    @@ Markup.create ?button_type:typ ?appearance ?dense ?icon ?label () in
  new t ?ripple ?on_click ?loader elt ()

let attach ?ripple ?loader ?on_click (elt : #Dom_html.element Js.t) : t =
  new t ?ripple ?loader ?on_click elt ()
