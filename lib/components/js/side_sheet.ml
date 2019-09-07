open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Side_sheet
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

type slide =
  [ `Leading
  | `Trailing ]

let equal_slide (a : slide) (b : slide) : bool =
  match a, b with
  | `Leading, `Leading | `Trailing, `Trailing -> true
  | _, _ -> false

module type M = sig
  include Components_tyxml.Side_sheet.Common_css

  val name : string

  val slide : slide
end

module Make_parent (M : M) = struct
  module Event = struct
    class type change = [unit] Dom_html.customEvent

    let open_ : change Js.t Dom_html.Event.typ =
      Dom_html.Event.make (Printf.sprintf "%s:open" M.name)

    let close : change Js.t Dom_html.Event.typ =
      Dom_html.Event.make (Printf.sprintf "%s:close" M.name)
  end

  class t (elt : Dom_html.element Js.t) () =
    object (self)
      val mutable _previous_focus = None

      val mutable _animation_thread = None

      val mutable _keydown_listener = None

      val mutable _scrim_click_listener = None

      val mutable _quick_open = false

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        let typ =
          if self#modal
          then Modal
          else if self#dismissible
          then Dismissible
          else Permanent
        in
        (match typ with
        | Modal -> self#set_modal ()
        | Permanent -> self#set_permanent ()
        | Dismissible -> self#set_dismissible ());
        (* Connect event listeners *)
        let keydown =
          Js_of_ocaml_lwt.Lwt_js_events.keydowns super#root self#handle_keydown
        in
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
        super#remove_class M.opening

      method set_quick_open x = _quick_open <- x

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

      method dismissible : bool = super#has_class M.dismissible

      method set_dismissible () : unit =
        super#remove_class M.modal;
        super#add_class M.dismissible;
        Option.iter Lwt.cancel _keydown_listener;
        _keydown_listener <- None;
        Option.iter Lwt.cancel _scrim_click_listener;
        _scrim_click_listener <- None

      method modal : bool = super#has_class M.modal

      method set_modal ?scrim () : unit =
        super#remove_class M.dismissible;
        super#add_class M.modal;
        let scrim =
          match scrim with
          | Some x -> Some x
          | None -> (
            match Js.Opt.to_option @@ Element.get_parent elt with
            | None -> None
            | Some p -> Element.query_selector p ("." ^ M.scrim))
        in
        match scrim with
        | None -> ()
        | Some scrim ->
            let listener =
              Js_of_ocaml_lwt.Lwt_js_events.clicks scrim self#handle_scrim_click
            in
            _scrim_click_listener <- Some listener

      method is_open : bool = super#has_class M.open_
      (** Returns [true] if drawer is in open state *)

      method toggle ?(force : bool option) () : unit Lwt.t =
        let v =
          match force with
          | None -> not self#is_open
          | Some x -> x
        in
        if not self#permanent
        then if v then self#show () else self#hide ()
        else Lwt.return_unit
      (** Toggles the drawer open and closed *)

      (* Private methods *)
      method private show () : unit Lwt.t =
        if (not self#permanent)
           && (not self#is_open)
           && (not self#is_opening)
           && not self#is_closing
        then (
          super#add_class M.open_;
          self#save_focus ();
          if not _quick_open
          then (
            super#add_class M.animate;
            Option.iter Lwt.cancel _animation_thread;
            Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame ()
            >>= Js_of_ocaml_lwt.Lwt_js.yield
            >>= fun () ->
            let waiter =
              Lwt.catch
                (fun () ->
                  Js_of_ocaml_lwt.Lwt_js_events.(
                    seq_loop
                      (make_event (Dom_html.Event.make "transitionend"))
                      super#root
                      (self#handle_transition_end ~closing:false % Option.some)))
                (function
                  | Lwt.Canceled -> Lwt.return_unit
                  | exn -> Lwt.fail exn)
            in
            super#add_class M.opening;
            waiter)
          else self#handle_transition_end ~closing:false None Lwt.return_unit)
        else Lwt.return_unit

      method private hide () : unit Lwt.t =
        if (not self#permanent)
           && self#is_open
           && (not self#is_opening)
           && not self#is_closing
        then
          if not _quick_open
          then (
            let waiter =
              Lwt.catch
                (fun () ->
                  Js_of_ocaml_lwt.Lwt_js_events.(
                    seq_loop
                      (make_event (Dom_html.Event.make "transitionend"))
                      super#root
                      (self#handle_transition_end ~closing:true % Option.some)))
                (function
                  | Lwt.Canceled -> Lwt.return_unit
                  | exn -> Lwt.fail exn)
            in
            super#add_class M.closing;
            waiter)
          else self#handle_transition_end ~closing:true None Lwt.return_unit
        else Lwt.return_unit

      method private notify_open () : unit = super#emit ~should_bubble:true Event.open_

      method private notify_close () : unit = super#emit ~should_bubble:true Event.close

      method private handle_scrim_click _ _ : unit Lwt.t = self#hide ()

      method private save_focus () : unit =
        _previous_focus <- Js.Opt.to_option Dom_html.document##.activeElement

      method private restore_focus () : unit =
        match _previous_focus with
        | None -> ()
        | Some elt ->
            if Js.to_bool @@ (Js.Unsafe.coerce self#root)##contains elt then elt##focus

      method private focus_active_navigation_item () : unit =
        (* TODO improve query *)
        let query = Js.string "a, button, input" in
        Js.Opt.iter (self#root##querySelector query) (fun e -> e##focus)

      method private is_opening : bool =
        super#has_class M.opening || super#has_class M.animate
      (** Returns [true] if drawer is animating open *)

      method private is_closing : bool = super#has_class M.closing
      (** Returns [true] if drawer is animating closed *)

      method private handle_keydown
          (e : Dom_html.keyboardEvent Js.t)
          (_ : unit Lwt.t)
          : unit Lwt.t =
        match Dom_html.Keyboard_code.of_event e with
        | Escape -> self#hide ()
        | _ -> Lwt.return_unit

      method private handle_transition_end
          ~closing
          (e : #Dom_html.event Js.t option)
          (t : unit Lwt.t)
          : unit Lwt.t =
        let is_root =
          match e with
          | None -> true
          | Some e -> Element.has_class (Dom.eventTarget e) M.root
        in
        try
          if is_root
          then (
            if closing
            then (
              super#remove_class M.open_;
              self#restore_focus ();
              self#notify_close ())
            else (
              self#focus_active_navigation_item ();
              self#notify_open ());
            super#remove_class M.animate;
            super#remove_class M.opening;
            super#remove_class M.closing;
            Lwt.cancel t);
          Lwt.return_unit
        with Not_found -> Lwt.return_unit

      method private get_delta ~x ~touch =
        match M.slide with
        | `Leading -> x - touch##.clientX
        | `Trailing -> touch##.clientX - x
    end
end

module Parent = Make_parent (struct
  include CSS

  let name = "side_sheet"

  let slide = `Trailing
end)

include (Parent : module type of Parent)

(** Creates new widget from scratch *)
let make ?classes ?attrs content : t =
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Markup_js.create
         ?classes
         ?attrs
         ~content:(List.map Tyxml_js.Of_dom.of_element content)
         ()
  in
  new t elt ()

(** Attach widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()
