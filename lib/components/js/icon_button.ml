open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Icon_button
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( >>= ) = Lwt.bind

module Attr = struct
  let aria_pressed = "aria-pressed"
end

module Event = struct
  class type change = [bool] Dom_html.customEvent

  let change : change Js.t Dom_html.Event.typ = Dom_html.Event.make (CSS.root ^ ":change")
end

module Lwt_js_events = struct
  open Js_of_ocaml_lwt.Lwt_js_events

  let change ?use_capture ?passive t = make_event ?use_capture ?passive Event.change t

  let changes ?cancel_handler ?use_capture ?passive t =
    seq_loop change ?cancel_handler ?use_capture ?passive t
end

class t ?ripple ?loader ?on_change ?on_click (elt : Dom_html.element Js.t) () =
  object (self)
    val _has_on_icon = Js.Opt.test @@ elt##querySelector (Js.string @@ "." ^ CSS.icon_on)

    inherit Button.t ?ripple ?loader elt () as super

    method! init () : unit =
      super#set_attribute Attr.aria_pressed (string_of_bool self#on);
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ clicks super#root (fun e t ->
                if _has_on_icon then self#toggle ~notify:true ();
                match on_click with
                | Some f -> f e t
                | None -> Lwt.return_unit) ]
          @ listeners);
      super#initial_sync_with_dom ()

    method toggle ?(notify = false) ?(force : bool option) () : unit =
      super#toggle_class ?force CSS.on;
      super#set_attribute Attr.aria_pressed (string_of_bool self#on);
      if notify then self#notify_change ()

    method on : bool = super#has_class CSS.on

    (* Private methods *)
    method private notify_change () : unit =
      super#emit ~detail:self#on Event.change;
      Option.iter (fun f -> f self#on) on_change

    method! private create_ripple () : Ripple.t = Ripple.attach ~unbounded:true elt
  end

(** Attach icon button widget to existing DOM element *)
let attach ?on_change ?on_click (elt : #Dom_html.element Js.t) : t =
  new t ?on_change ?on_click (Element.coerce elt) ()

let make ?classes ?a ?ripple ?on ?disabled ?on_icon ?on_change ?on_click ~icon () =
  D.icon_button ?classes ?a ?ripple ?on ?disabled ?on_icon ~icon ()
  |> Tyxml_js.To_dom.of_button
  |> attach ?on_change ?on_click

let make_a ?classes ?a ?href ?ripple ?on ?on_icon ?on_change ?on_click ~icon () =
  D.icon_button_a ?classes ?a ?href ?ripple ?on ?on_icon ~icon ()
  |> Tyxml_js.To_dom.of_a
  |> attach ?on_change ?on_click
