open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml

include Components_tyxml.Icon_button
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let ( >>= ) = Lwt.bind

module Attr = struct
  let aria_pressed = "aria-pressed"
end

module Event = struct
  class type change =
    object
      inherit [bool] Widget.custom_event
    end

  let change : change Js.t Dom_html.Event.typ =
    Dom_html.Event.make "icon_button:change"
end

class t ?ripple ?loader ?on_change ?on_click (elt : Dom_html.element Js.t) () =
  object(self)
    val _has_on_icon = Js.Opt.test @@ elt##querySelector (Js.string @@ "." ^ CSS.icon_on)
    inherit Button.t ?ripple ?loader elt () as super

    method! init () : unit =
      super#init ();
      super#set_attribute Attr.aria_pressed (string_of_bool self#on)

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      listeners_ <- Lwt_js_events.(
          [ clicks super#root (fun e t ->
                if _has_on_icon then self#toggle ~notify:true ();
                match on_click with
                | Some f -> f e t
                | None -> Lwt.return_unit)
          ])

    method toggle ?(notify = false) ?(force : bool option) () : unit =
      super#toggle_class ?force CSS.on;
      super#set_attribute Attr.aria_pressed (string_of_bool self#on);
      if notify then self#notify_change ()

    method on : bool =
      super#has_class CSS.on

    (* Private methods *)

    method private notify_change () : unit =
      super#emit ~detail:self#on Event.change;
      Option.iter (fun f -> f self#on) on_change

    method! private create_ripple () : Ripple.t =
      Ripple.attach ~unbounded:true elt
  end

(** Create new icon button widget from scratch *)
let make ?(tag = `Button) ?classes ?href
    ?on ?ripple ?on_icon ?disabled ?on_change ?on_click
    ~icon () : t =
  Option.iter (fun i ->
      i#add_class CSS.icon;
      i#add_class CSS.icon_on) on_icon;
  Element.add_class icon CSS.icon;
  let elt =
    Tyxml_js.To_dom.of_element
    @@ match tag with
    | `Button ->
      Markup.create ?classes ?ripple ?on ?disabled
        ?on_icon:(Option.map Widget.to_markup on_icon)
        ~icon:(Tyxml_js.Of_dom.of_element icon)
        ()
    | `Anchor ->
      Markup.create_anchor ?classes ?ripple ?on ?href
        ?on_icon:(Option.map Widget.to_markup on_icon)
        ~icon:(Tyxml_js.Of_dom.of_element icon)
        () in
  new t ?on_change ?on_click elt ()

(** Attach icon button widget to existing DOM element *)
let attach ?on_change ?on_click (elt : #Dom_html.element Js.t) : t =
  new t ?on_change ?on_click (Element.coerce elt) ()
