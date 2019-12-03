open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Side_sheet
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

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

  val slide : slide
end

module Make_parent (M : M) = struct
  module Event = struct
    class type change = [unit] Dom_html.customEvent

    let open_ : change Js.t Dom_html.Event.typ =
      Dom_html.Event.make (Printf.sprintf "%s:open" M.root)

    let close : change Js.t Dom_html.Event.typ =
      Dom_html.Event.make (Printf.sprintf "%s:close" M.root)
  end

  module Lwt_js_events = struct
    open Js_of_ocaml_lwt.Lwt_js_events

    let open_ ?use_capture ?passive t = make_event ?use_capture ?passive Event.open_ t

    let opens ?cancel_handler ?use_capture ?passive t =
      seq_loop ?cancel_handler ?use_capture ?passive open_ t

    let close ?use_capture ?passive t = make_event ?use_capture ?passive Event.close t

    let closes ?cancel_handler ?use_capture ?passive t =
      seq_loop ?cancel_handler ?use_capture ?passive close t
  end

  class t (elt : Dom_html.element Js.t) () =
    object (self)
      val mutable previous_focus = None

      val mutable animation_thread = None

      val mutable quick_open = false

      val mutable keydown_listener = None

      val mutable scrim_click_listener = None

      inherit Widget.t elt () as super

      method! init () : unit =
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
        super#init ()

      method! destroy () : unit =
        (* Detach event listeners *)
        Option.iter Lwt.cancel keydown_listener;
        keydown_listener <- None;
        Option.iter Lwt.cancel scrim_click_listener;
        scrim_click_listener <- None;
        (* Clear animation *)
        Option.iter Lwt.cancel animation_thread;
        animation_thread <- None;
        (* Clear classes *)
        super#remove_class M.animate;
        super#remove_class M.closing;
        super#remove_class M.opening;
        super#destroy ()

      method set_quick_open x = quick_open <- x

      method permanent : bool =
        not (super#has_class M.modal || super#has_class M.dismissible)

      method set_permanent () : unit =
        super#remove_class M.modal;
        super#remove_class M.dismissible;
        super#remove_class M.animate;
        super#remove_class M.closing;
        super#remove_class M.opening;
        Option.iter Lwt.cancel keydown_listener;
        keydown_listener <- None;
        Option.iter Lwt.cancel scrim_click_listener;
        scrim_click_listener <- None

      method dismissible : bool = super#has_class M.dismissible

      method set_dismissible () : unit =
        super#remove_class M.modal;
        super#add_class M.dismissible;
        Option.iter Lwt.cancel keydown_listener;
        keydown_listener <- None;
        Option.iter Lwt.cancel scrim_click_listener;
        scrim_click_listener <- None

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
        (* Attach event listeners *)
        Js_of_ocaml_lwt.Lwt_js_events.(
          keydown_listener <- Some (keydowns super#root self#handle_keydown);
          Option.iter
            (fun scrim ->
              scrim_click_listener <- Some (clicks scrim self#handle_scrim_click))
            scrim)

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
          if not quick_open
          then (
            super#add_class M.animate;
            Option.iter Lwt.cancel animation_thread;
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
          if not quick_open
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
        previous_focus <- Js.Opt.to_option Dom_html.document##.activeElement

      method private restore_focus () : unit =
        match previous_focus with
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

  let slide = `Trailing
end)

include (Parent : module type of Parent)

(** Attach widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

let make ?classes ?a ?children () =
  D.side_sheet ?classes ?a ?children () |> Tyxml_js.To_dom.of_aside |> attach
