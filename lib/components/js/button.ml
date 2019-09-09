open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Button
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>= ) = Lwt.bind

class t ?(ripple = true) ?on_click ?loader (elt : Dom_html.element Js.t) () =
  object (self)
    val mutable loader_ = Option.map Widget.coerce loader

    val mutable loader_container_ : Dom_html.element Js.t option =
      Element.query_selector elt CSS.loader_container

    val mutable ripple_ : Ripple.t option = None

    val mutable listeners : unit Lwt.t list = []

    inherit Widget.t elt () as super

    method! init () : unit =
      if ripple then ripple_ <- Some (self#create_ripple ());
      super#init ()

    method! initial_sync_with_dom () : unit =
      (match on_click with
      | None -> ()
      | Some f ->
          listeners <-
            Js_of_ocaml_lwt.Lwt_js_events.(
              [clicks super#root (f (self :> t))] @ listeners));
      super#initial_sync_with_dom ()

    method! layout () : unit =
      (* Layout internal components. *)
      Option.iter Ripple.layout ripple_;
      Option.iter Widget.layout loader_;
      super#layout ()

    method! destroy () : unit =
      (* Destroy internal components. *)
      Option.iter Ripple.destroy ripple_;
      ripple_ <- None;
      Option.iter Widget.destroy loader_;
      loader_ <- None;
      List.iter Lwt.cancel listeners;
      listeners <- [];
      super#destroy ()

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

    method set_loader : 'a. (#Widget.t as 'a) -> unit =
      fun loader -> loader_ <- Some loader#widget

    method loading : bool = super#has_class CSS.loading

    method set_loading_lwt : 'a. 'a Lwt.t -> unit =
      fun t ->
        self#set_loading true;
        Lwt.on_termination t (fun () -> self#set_loading false)

    method set_loading (x : bool) : unit =
      if x
      then (
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
                | Some x -> x
              in
              let container =
                Tyxml_js.To_dom.of_element
                @@ Markup_js.create_loader_container (Widget.markup loader) ()
              in
              loader_container_ <- Some container;
              container
        in
        Element.append_child super#root loader_container)
      else (
        super#remove_class CSS.loading;
        self#set_disabled false;
        Option.iter (Element.remove_child_safe super#root) loader_container_)

    method private create_ripple () : Ripple.t = Ripple.attach super#root
  end

let attach ?ripple ?loader ?on_click (elt : #Dom_html.element Js.t) : t =
  let make elt = new t ?ripple ?loader ?on_click (Element.coerce elt) () in
  Js.Opt.case
    (Dom_html.CoerceTo.button elt)
    (fun () ->
      Js.Opt.case
        (Dom_html.CoerceTo.a elt)
        (fun () ->
          failwith (CSS.root ^ ": root element should have `a` or `button` tag"))
        make)
    make

let make
    ?classes
    ?attrs
    ?ripple
    ?loader
    ?on_click
    ?button_type
    ?appearance
    ?dense
    ?icon
    ?label
    () =
  Markup_js.create ?classes ?attrs ?button_type ?appearance ?dense ?icon ?label ()
  |> Tyxml_js.To_dom.of_button
  |> attach ?ripple ?loader ?on_click

let make_anchor
    ?classes
    ?attrs
    ?ripple
    ?loader
    ?on_click
    ?appearance
    ?href
    ?dense
    ?icon
    ?label
    () =
  Markup_js.create_anchor ?classes ?attrs ?appearance ?href ?dense ?icon ?label ()
  |> Tyxml_js.To_dom.of_a
  |> attach ?ripple ?loader ?on_click
