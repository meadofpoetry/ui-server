open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

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

  let change : change Js.t Events.Typ.t =
    Events.Typ.make "icon_button:change"
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
      let listener =
        Events.listen_lwt super#root Events.Typ.click (fun e t ->
            if _has_on_icon then self#toggle ~notify:true ();
            match on_click with
            | Some f -> f e t
            | None -> Lwt.return_unit) in
      _click_listener <- Some listener

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
let make ?(tag = `Button) ?href
    ?on ?ripple ?on_icon ?disabled ?on_change ?on_click ~icon () : t =
  Option.iter (fun i ->
      i#add_class CSS.icon;
      i#add_class CSS.icon_on) on_icon;
  icon#add_class CSS.icon;
  let elt =
    Tyxml_js.To_dom.of_element
    @@ match tag with
    | `Button ->
      Markup.create ?ripple ?on ?disabled
        ?on_icon:(Option.map Widget.to_markup on_icon)
        ~icon:(Widget.to_markup icon)
        ()
    | `Anchor ->
      Markup.create_anchor ?ripple ?on ?href
        ?on_icon:(Option.map Widget.to_markup on_icon)
        ~icon:(Widget.to_markup icon)
        () in
  new t ?on_change ?on_click elt ()

(** Attach icon button widget to existing DOM element *)
let attach ?on_change ?on_click (elt : #Dom_html.element Js.t) : t =
  new t ?on_change ?on_click (Element.coerce elt) ()
