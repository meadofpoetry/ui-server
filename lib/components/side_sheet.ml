open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Utils

include Components_tyxml.Side_sheet
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let ( >>= ) = Lwt.bind

type slide = [`Leading | `Trailing]

let equal_slide (a : slide) (b : slide) : bool =
  match a, b with
  | `Leading, `Leading | `Trailing, `Trailing -> true
  | _, _ -> false

module type M = sig
  include Components_tyxml.Side_sheet.Common_css
  val name : string
  val slide : slide
end

module Make_parent(M : M) = struct

  module Scrim = struct
    class t (elt : Dom_html.element Js.t) () =
    object
      inherit Widget.t elt ()
    end

    (** Creates new widget from scratch *)
    let make () : t =
      let (elt : Dom_html.element Js.t) =
        Tyxml_js.To_dom.of_element
        @@ Markup.create_scrim () in
      new t elt ()

    (** Attach widget to existing element *)
    let attach (elt : #Dom_html.element Js.t) : t =
      new t (Element.coerce elt) ()
  end

  module Event = struct
    class type change =
      object
        inherit [unit] Widget.custom_event
      end

    let open_ : change Js.t Events.Typ.t =
      Events.Typ.make (Printf.sprintf "%s:open" M.name)

    let close : change Js.t Events.Typ.t =
      Events.Typ.make (Printf.sprintf "%s:close" M.name)
  end

  let get_target (e : #Dom_html.event Js.t) : Dom_html.element Js.t =
    Js.Opt.get (e##.target) (fun () -> raise Not_found)

  class t (elt : Dom_html.element Js.t) () =
  object(self)
    val mutable _previous_focus = None

    (* Animation *)
    val mutable _animation_thread = None
    (* Event listeners *)
    val mutable _keydown_listener = None
    val mutable _scrim_click_listener = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      let typ =
        if self#modal
        then Modal
        else if self#dismissible
        then Dismissible
        else Permanent in
      begin match typ with
      | Modal -> self#set_modal ()
      | Permanent -> self#set_permanent ()
      | Dismissible -> self#set_dismissible ()
      end;
      (* Connect event listeners *)
      let keydown = Events.keydowns super#root self#handle_keydown in
      _keydown_listener <- Some keydown

    method! destroy () : unit =
      super#destroy ();
      (* Detach event listeners *)
      Option.iter Lwt.cancel _keydown_listener;
      _keydown_listener <- None;
      Option.iter Lwt.cancel _scrim_click_listener;
      _scrim_click_listener <- None;
      (* Clear animation *)
      Option.iter Lwt.cancel _animation_thread;
      _animation_thread <- None;
      (* Clear classes *)
      super#remove_class M.animate;
      super#remove_class M.closing;
      super#remove_class M.opening;

    method permanent : bool =
      not (super#has_class M.modal || super#has_class M.dismissible)

    method set_permanent () : unit =
      super#remove_class M.modal;
      super#remove_class M.dismissible;
      super#remove_class M.animate;
      super#remove_class M.closing;
      super#remove_class M.opening;
      Option.iter Lwt.cancel _keydown_listener;
      _keydown_listener <- None;
      Option.iter Lwt.cancel _scrim_click_listener;
      _scrim_click_listener <- None

    method dismissible : bool =
      super#has_class M.dismissible

    method set_dismissible () : unit =
      super#remove_class M.modal;
      super#add_class M.dismissible;
      Option.iter Lwt.cancel _keydown_listener;
      _keydown_listener <- None;
      Option.iter Lwt.cancel _scrim_click_listener;
      _scrim_click_listener <- None

    method modal : bool =
      super#has_class M.modal

    method set_modal ?scrim () : unit =
      super#remove_class M.dismissible;
      super#add_class M.modal;
      let scrim = match scrim with
        | Some x -> Some x
        | None ->
           match Js.Opt.to_option @@ Element.get_parent elt with
           | None -> None
           | Some p -> Element.query_selector p ("." ^ M.scrim) in
      match scrim with
      | None -> ()
      | Some scrim ->
         let listener =
           Events.listen_lwt scrim Events.Typ.click (fun _ _ ->
               self#handle_scrim_click ()) in
         _scrim_click_listener <- Some listener

    (** Returns [true] if drawer is in open state *)
    method is_open : bool =
      super#has_class M.open_

    (** Toggles the drawer open and closed *)
    method toggle ?(force : bool option) () : unit Lwt.t =
      let v = match force with None -> not self#is_open | Some x -> x in
      if not self#permanent
      then if v then self#show () else self#hide ()
      else Lwt.return_unit

    (* Private methods *)

    method private show () : unit Lwt.t =
      if not self#permanent
         && not self#is_open
         && not self#is_opening
         && not self#is_closing
      then (
        super#add_class M.open_;
        super#add_class M.animate;
        self#save_focus ();
        Option.iter Lwt.cancel _animation_thread;
        Animation.request ()
        >>= fun _ -> Lwt_js.yield ()
        >>= (fun () ->
          super#add_class M.opening;
          Lwt.catch (fun () ->
              Events.listen_lwt super#root
                (Events.Typ.make "transitionend")
                self#handle_transition_end)
            (function
             | Lwt.Canceled -> Lwt.return_unit
             | exn -> Lwt.fail exn)))
      else Lwt.return_unit

    method private hide () : unit Lwt.t =
      if not self#permanent
         && self#is_open
         && not self#is_opening
         && not self#is_closing
      then (super#add_class M.closing;
            Lwt.catch (fun () ->
                Events.listen_lwt super#root
                  (Events.Typ.make "transitionend")
                  self#handle_transition_end)
              (function
               | Lwt.Canceled -> Lwt.return_unit
               | exn -> Lwt.fail exn))
      else Lwt.return_unit

    method private notify_open () : unit =
      super#emit ~should_bubble:true Event.open_

    method private notify_close () : unit =
      super#emit ~should_bubble:true Event.close

    method private handle_scrim_click () : unit Lwt.t =
      self#hide ()

    method private save_focus () : unit =
      _previous_focus <- Js.Opt.to_option Dom_html.document##.activeElement

    method private restore_focus () : unit =
      match _previous_focus with
      | None -> ()
      | Some elt ->
         if Js.to_bool @@ (Js.Unsafe.coerce self#root)##contains elt
         then elt##focus

    method private focus_active_navigation_item () : unit =
      (* TODO improve query *)
      let query = Js.string "a, button, input" in
      Js.Opt.iter (self#root##querySelector query) (fun e -> e##focus)

    (** Returns [true] if drawer is animating open *)
    method private is_opening : bool =
      super#has_class M.opening || super#has_class M.animate

    (** Returns [true] if drawer is animating closed *)
    method private is_closing : bool =
      super#has_class M.closing

    method private handle_keydown (e : Dom_html.keyboardEvent Js.t)
                     (_ : unit Lwt.t) : unit Lwt.t =
      match Events.Key.of_event e with
      | `Escape -> self#hide ()
      | _ -> Lwt.return_unit

    method private handle_transition_end (e : #Dom_html.event Js.t)
                     (t : unit Lwt.t) : unit Lwt.t =
      try
        if Element.has_class (get_target e) M.root
        then begin
            if self#is_closing
            then (super#remove_class M.open_;
                  self#restore_focus ();
                  self#notify_close ())
            else (self#focus_active_navigation_item ();
                  self#notify_open ());
            super#remove_class M.animate;
            super#remove_class M.opening;
            super#remove_class M.closing;
            Lwt.cancel t;
          end;
        Lwt.return_unit
      with Not_found -> Lwt.return_unit

    method private get_delta ~x ~touch =
      match M.slide with
      | `Leading -> x - touch##.clientX
      | `Trailing -> touch##.clientX - x
  end
end

module Parent =
  Make_parent(struct
      include CSS
      let name = "side_sheet"
      let slide = `Trailing
    end)

class t (elt : Dom_html.element Js.t) () =
object
  inherit Parent.t elt ()
end

include (Parent : module type of Parent with type t := t)

let make_element widgets =
  Tyxml_js.To_dom.of_element
  @@ Markup.create (List.map Widget.to_markup widgets) ()

(** Creates new widget from scratch *)
let make (widgets : #Widget.t list) () : t =
  let elt = make_element widgets in
  new t elt ()

(** Attach widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
