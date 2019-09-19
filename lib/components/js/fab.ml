open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Fab
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( >>= ) = Lwt.bind

class t ?on_click (elt : Dom_html.buttonElement Js.t) () =
  object (self)
    val mutable ripple : Ripple.t option = None

    val mutable listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      ripple <- Some (self#create_ripple ())

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      match on_click with
      | None -> ()
      | Some f ->
          listeners <-
            Js_of_ocaml_lwt.Lwt_js_events.(
              [clicks super#root (f (self :> t))] @ listeners)

    method! layout () : unit =
      Option.iter Ripple.layout ripple;
      super#layout ()

    method! destroy () : unit =
      (* Detach event listeners *)
      List.iter Lwt.cancel listeners;
      listeners <- [];
      (* Destroy internal components *)
      Option.iter (fun r -> r#destroy ()) ripple;
      ripple <- None;
      super#destroy ()

    method mini : bool = super#has_class CSS.mini

    method set_mini (x : bool) : unit = super#toggle_class ~force:x CSS.mini

    method private create_ripple () : Ripple.t = Ripple.attach super#root
  end

let attach ?on_click (elt : #Dom_html.element Js.t) : t =
  Js.Opt.case
    (Dom_html.CoerceTo.button elt)
    (fun () -> failwith (CSS.root ^ ": root element must have a `button` tag"))
    (fun elt -> new t ?on_click elt ())

let make ?classes ?a ?mini ?extended ?label ?icon ?on_click () : t =
  D.fab ?classes ?a ?mini ?extended ?label ?icon ()
  |> Tyxml_js.To_dom.of_button
  |> attach ?on_click
