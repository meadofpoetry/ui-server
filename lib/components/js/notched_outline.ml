open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Notched_outline
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>= ) = Lwt.bind

module Const = struct
  let padding = 8
end

class t (elt : Dom_html.element Js.t) () =
  object
    val notch_elt = Element.query_selector elt ("." ^ CSS.notch)

    inherit Widget.t elt () as super

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      match Element.query_selector elt ("." ^ Floating_label.CSS.root) with
      | None -> super#add_class CSS.no_label
      | Some label ->
          (Js.Unsafe.coerce label##.style)##.transitionDuration := Js.string "0s";
          super#add_class CSS.upgraded;
          Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame ()
          >>= (fun () ->
                (Js.Unsafe.coerce label##.style)##.transitionDuration := Js.string "";
                Lwt.return_unit)
          |> Lwt.ignore_result

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

let make ?label () : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element @@ Markup_js.create ?label ()
  in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()
