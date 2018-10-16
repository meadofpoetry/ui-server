open Containers
open Tyxml_js

module Markup = Components_markup.Line_ripple.Make(Xml)(Svg)(Html)

class t () =
  let elt = Markup.create () |> To_dom.of_element in
  object(self)

    val mutable _transitionend_listener = None

    inherit Widget.t elt () as super

    (* Activates the line ripple *)
    method activate () : unit =
      self#remove_class Markup.deactivating_class;
      self#add_class Markup.active_class

    (* Deactivates the line ripple *)
    method deactivate () : unit =
      self#add_class Markup.deactivating_class

    (* Sets the center of the ripple animation to the given X coordinate. *)
    method set_ripple_center (x_coordinate : int) : unit =
      let value = Js.string @@ Printf.sprintf "%dpx center" x_coordinate in
      (Js.Unsafe.coerce self#style)##.transformOrigin := value

    method init () : unit =
      self#listen_lwt (Widget.Event.make "transitionend") (fun e _ ->
          Lwt.return @@ self#handle_transition_end e)
      |> fun x -> _transitionend_listener <- Some x

    method destroy () : unit =
      super#destroy ();
      Option.iter Lwt.cancel _transitionend_listener;
      _transitionend_listener <- None

    (* Private methods *)

    (* Handles a transition end event *)
    method private handle_transition_end (e : Dom_html.event Js.t) : unit =
      let is_deactivating = self#has_class Markup.deactivating_class in
      let prop = Js.to_string (Js.Unsafe.coerce e)##.propertyName in
      match prop with
      | "opacity" ->
         if is_deactivating
         then
           (self#remove_class Markup.active_class;
            self#remove_class Markup.deactivating_class)
      | _ -> ()

    initializer
      self#init ()

  end
