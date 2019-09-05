open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Fab
module Markup = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>= ) = Lwt.bind

class t ?on_click (elt : Dom_html.buttonElement Js.t) () =
  object (self)
    val mutable _ripple : Ripple.t option = None

    val mutable _click_listener = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      _ripple <- Some (self#create_ripple ())

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      match on_click with
      | None -> ()
      | Some f ->
          let listener =
            Js_of_ocaml_lwt.Lwt_js_events.clicks super#root (f (self :> t))
          in
          _click_listener <- Some listener

    method! layout () : unit =
      super#layout ();
      Option.iter Ripple.layout _ripple

    method! destroy () : unit =
      super#destroy ();
      (* Destroy internal components *)
      Option.iter (fun r -> r#destroy ()) _ripple;
      _ripple <- None

    method mini : bool = super#has_class CSS.mini

    method set_mini (x : bool) : unit = super#toggle_class ~force:x CSS.mini

    (* Private methods *)
    method private create_ripple () : Ripple.t = Ripple.attach super#root
  end

let make ?mini ?extended ?label ?icon ?on_click () : t =
  let icon =
    match icon with
    | None -> None
    | Some x -> Some (Widget.to_markup x)
  in
  let (elt : Dom_html.buttonElement Js.t) =
    Tyxml_js.To_dom.of_button @@ Markup.create ?mini ?extended ?label ?icon ()
  in
  new t ?on_click elt ()

let attach ?on_click (elt : #Dom_html.element Js.t) : t =
  Js.Opt.case
    (Dom_html.CoerceTo.button elt)
    (fun () -> failwith "FAB: host element must have a `button` tag")
    (fun elt -> new t ?on_click elt ())
