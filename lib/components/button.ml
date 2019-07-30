open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Utils

include Components_tyxml.Button
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let ( >>= ) = Lwt.bind

class t ?(ripple = true) ?on_click ?loader (elt : Dom_html.element Js.t) () =
  object(self)
    val mutable loader_ = Option.map Widget.coerce loader
    val mutable loader_container_ : Dom_html.element Js.t option =
      Element.query_selector elt CSS.loader_container
    val mutable ripple_ : Ripple.t option = None
    val mutable listeners_ : unit Lwt.t list = []

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      if ripple then ripple_ <- Some (self#create_ripple ())

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      match on_click with
      | None -> ()
      | Some f -> listeners_ <- Lwt_js_events.(
          [ clicks super#root (f (self :> t))
          ])

    method! layout () : unit =
      super#layout ();
      (* Layout internal components. *)
      Option.iter Ripple.layout ripple_;
      Option.iter Widget.layout loader_

    method! destroy () : unit =
      super#destroy ();
      (* Destroy internal components. *)
      Option.iter Ripple.destroy ripple_;
      ripple_ <- None;
      Option.iter Widget.destroy loader_;
      loader_ <- None;
      List.iter Lwt.cancel listeners_;
      listeners_ <- []

    method disabled : bool =
      match Js.to_string elt##.tagName with
      | "BUTTON" ->
        let (button : Dom_html.buttonElement Js.t) = Js.Unsafe.coerce elt in
        Js.to_bool button##.disabled
      | _ -> false

    method set_disabled (x : bool) : unit =
      match Js.to_string elt##.tagName with
      | "BUTTON" ->
        let (button : Dom_html.buttonElement Js.t) = Js.Unsafe.coerce elt in
        button##.disabled := Js.bool x
      | _ -> ()

    method set_loader : 'a. (#Widget.t as 'a) -> unit = fun loader ->
      loader_ <- Some (loader#widget)

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
          match loader_container_ with
          | Some x -> x
          | None ->
            let (loader : Widget.t) =
              match loader_ with
              | None ->
                let progress = (Circular_progress.make ~size:25 ())#widget in
                loader_ <- Some progress;
                progress
              | Some x -> x in
            let container =
              Tyxml_js.To_dom.of_element
              @@ Markup.create_loader_container (Widget.to_markup loader) () in
            loader_container_ <- Some container;
            container in
        Element.append_child super#root loader_container)
      else (
        super#remove_class CSS.loading;
        self#set_disabled false;
        Option.iter (Element.remove_child_safe super#root) loader_container_)

    method private create_ripple () : Ripple.t =
      Ripple.attach super#root
  end

let make ?classes ?(tag = `Button) ?typ ?href
    ?appearance ?icon ?dense ?ripple ?label ?loader ?on_click () : t =
  let icon = match icon with
    | None -> None
    | Some (i : #Widget.t) ->
      i#add_class CSS.icon;
      Some (Widget.to_markup i) in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ match tag with
    | `Button ->
      Markup.create ?classes ?button_type:typ ?appearance ?dense ?icon ?label ()
    | `Anchor ->
      Markup.create_anchor ?classes ?appearance ?href ?dense ?icon ?label () in
  new t ?ripple ?on_click ?loader elt ()

let attach ?ripple ?loader ?on_click (elt : #Dom_html.element Js.t) : t =
  new t ?ripple ?loader ?on_click (Element.coerce elt) ()
