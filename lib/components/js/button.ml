open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Button
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( >>= ) = Lwt.bind

module Selector = struct
  let loader_container = "." ^ CSS.loader_container
end

class t ?(ripple = true) ?on_click ?loader (elt : Dom_html.element Js.t) () =
  object (self)
    val mutable loader = loader

    val mutable loader_container : Dom_html.element Js.t option =
      Element.query_selector elt Selector.loader_container

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
      super#layout ()

    method! destroy () : unit =
      (* Destroy internal components. *)
      Option.iter Ripple.destroy ripple_;
      ripple_ <- None;
      List.iter Lwt.cancel listeners;
      listeners <- [];
      super#destroy ()

    method disabled : bool =
      Js.Opt.case
        (Dom_html.CoerceTo.button super#root)
        (fun () -> false)
        (fun button -> Js.to_bool button##.disabled)

    method set_disabled (x : bool) : unit =
      Js.Opt.iter (Dom_html.CoerceTo.button super#root) (fun button ->
          button##.disabled := Js.bool x)

    method set_loader x = loader <- Some (Tyxml_js.Of_dom.of_element x)

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
          match loader_container with
          | Some x -> x
          | None ->
              let loader =
                match loader with
                | None ->
                    let progress = Circular_progress.D.circular_progress ~size:25 () in
                    loader <- Some progress;
                    progress
                | Some x -> x
              in
              let container =
                Tyxml_js.To_dom.of_element
                @@ D.button_loader_container ~children:[loader] ()
              in
              loader_container <- Some container;
              container
        in
        Element.append_child super#root loader_container)
      else (
        super#remove_class CSS.loading;
        self#set_disabled false;
        Option.iter (Element.remove_child_safe super#root) loader_container)

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
    ?a
    ?ripple
    ?loader
    ?on_click
    ?button_type
    ?appearance
    ?dense
    ?icon
    ?label
    () =
  D.button ?classes ?a ?button_type ?appearance ?dense ?icon ?label ()
  |> Tyxml_js.To_dom.of_button
  |> attach ?ripple ?loader ?on_click

let make_a ?classes ?a ?ripple ?loader ?on_click ?appearance ?href ?dense ?icon ?label ()
    =
  D.button_a ?classes ?a ?appearance ?href ?dense ?icon ?label ()
  |> Tyxml_js.To_dom.of_a
  |> attach ?ripple ?loader ?on_click
