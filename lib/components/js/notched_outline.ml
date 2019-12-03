open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Notched_outline
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( >>= ) = Lwt.bind

module Const = struct
  let padding = 8
end

module Selector = struct
  let floating_label = "." ^ Floating_label.CSS.root
end

class t (elt : Dom_html.element Js.t) () =
  object
    val notch_elt = Element.query_selector elt ("." ^ CSS.notch)

    inherit Widget.t elt () as super

    method! initial_sync_with_dom () : unit =
      (match Element.query_selector elt Selector.floating_label with
      | None -> super#add_class CSS.no_label
      | Some label ->
          (Js.Unsafe.coerce label##.style)##.transitionDuration := Js.string "0s";
          super#add_class CSS.upgraded;
          Lwt.async (fun () ->
              Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame ()
              >>= fun () ->
              (Js.Unsafe.coerce label##.style)##.transitionDuration := Js.string "";
              Lwt.return_unit));
      super#initial_sync_with_dom ()

    method notch (notch_width : float) : unit =
      super#add_class CSS.notched;
      match notch_elt with
      | None -> ()
      | Some notch ->
          let notch_width =
            match notch_width with
            | x when x > 0. -> x +. float_of_int Const.padding
            | x -> x
          in
          let px = Printf.sprintf "%gpx" notch_width in
          notch##.style##.width := Js.string px
    (** Adds the outline notched selector and updates the notch width
      calculated based off of notch_width *)

    method close_notch () : unit =
      super#remove_class CSS.notched;
      match notch_elt with
      | None -> ()
      | Some notch -> notch##.style##.width := Js.string ""
    (** Removes notched outline selector to close the notch in the outline *)
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

let make ?classes ?a ?leading ?trailing ?notch ?label_for ?label ?children () : t =
  D.notched_outline ?classes ?a ?leading ?trailing ?notch ?label_for ?label ?children ()
  |> Tyxml_js.To_dom.of_div
  |> attach
