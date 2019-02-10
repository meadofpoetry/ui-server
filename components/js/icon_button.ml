open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_tyxml.Icon_button.Make(Xml)(Svg)(Html)

class t (elt : #Dom_html.buttonElement Js.t) () =
  let e_state, set_state = React.E.create () in
  object(self)

    val mutable s_state = None
    val mutable on_change = None
    val mutable _ripple : Ripple.t option = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ()

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      let ripple = match Element.get_attribute elt "data-ripple" with
        | Some "true" -> true | _ -> false in
      if ripple
      then _ripple <- Some (Ripple.attach ~unbounded:true elt);
      match Element.query_selector elt ("." ^ Markup.CSS.icon_on) with
      | None -> ()
      | Some _ ->
         super#listen_lwt' Widget.Event.click (fun _ _ ->
             self#toggle (); Lwt.return_unit)

    method! layout () : unit =
      super#layout ();
      Option.iter Ripple.layout _ripple

    method! destroy () : unit =
      super#destroy ();
      Option.iter Ripple.destroy _ripple;
      _ripple <- None;
      Option.iter (React.S.stop ~strong:true) s_state;
      s_state <- None;
      React.E.stop ~strong:true e_state

    method set_on_change (f : bool -> unit) : unit =
      on_change <- Some f

    method disabled : bool =
      Js.to_bool elt##.disabled

    method set_disabled (x : bool) : unit =
      elt##.disabled := Js.bool x

    method toggle () : unit =
      self#set_on (not self#on);
      set_state self#on;
      Option.iter (fun f -> f self#on) on_change

    method e_state : bool React.event =
      e_state

    method s_state : bool React.signal =
      match s_state with
      | Some s -> s
      | None ->
         let s = React.S.hold ~eq:Equal.bool self#on e_state in
         s_state <- Some s;
         s

    method on : bool =
      super#has_class Markup.CSS.on

    method set_on (x : bool) : unit =
      if not @@ Equal.bool self#on x
      then (
        Option.iter (fun f -> f x) on_change;
        set_state x;
        super#toggle_class ~force:x Markup.CSS.on)

  end

(** Create new icon button widget from scratch *)
let make ?on ?ripple ?on_change ?on_icon ?disabled ~icon () : t =
  Option.iter (fun i ->
      i#add_class Markup.CSS.icon;
      i#add_class Markup.CSS.icon_on) on_icon;
  icon#add_class Markup.CSS.icon;
  let elt =
    To_dom.of_button
    @@ Markup.create ?ripple ?on ?disabled
         ?on_icon:(Option.map Widget.to_markup on_icon)
         ~icon:(Widget.to_markup icon)
         () in
  let t = new t elt () in
  Option.iter t#set_on_change on_change;
  t

(** Attach icon button widget to existing DOM element *)
let attach (elt : #Dom_html.element Js.t) : t =
  match Js.to_string elt##.tagName with
  | "BUTTON" -> new t (Js.Unsafe.coerce elt) ()
  | _ -> failwith "Icon button: host element must have a `button` tag"
