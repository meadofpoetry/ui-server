open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Utils

let ( >>= ) = Lwt.bind

include Components_tyxml.Dialog
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let are_tops_misaligned (elts : Dom_html.element Js.t list) : bool =
  List.map (fun (e : Dom_html.element Js.t) -> e##.offsetTop) elts
  |> List.sort_uniq compare
  |> function [] | [_] -> false | _ -> true

let reverse_elements (elts : Dom_html.element Js.t list) :
  Dom_html.element Js.t list =
  let lst = List.rev elts in
  List.iter (fun (elt : Dom_html.element Js.t) ->
      let parent = Element.get_parent elt in
      Js.Opt.iter parent (fun parent ->
          Element.append_child parent elt)) lst;
  lst

module Event = struct
  let opening : unit Widget.custom_event Js.t Dom_html.Event.typ =
    Dom_html.Event.make "opening"
  let opened : unit Widget.custom_event Js.t Dom_html.Event.typ =
    Dom_html.Event.make "opened"
  let closing : action Widget.custom_event Js.t Dom_html.Event.typ =
    Dom_html.Event.make "closing"
  let closed : action Widget.custom_event Js.t Dom_html.Event.typ =
    Dom_html.Event.make "closed"
end

module Const = struct
  let animation_close_time_s = 0.075
  let animation_open_time_s = 0.150
end

module Attr = struct
  let action = "data-mdc-dialog-action"
end

module Selector = struct
  let scrim = "." ^ CSS.scrim
  let content = "." ^ CSS.content
  let button = "." ^ CSS.button
  let default_button = "." ^ CSS.button_default
  let suppress_default_press =
    String.concat ", "
      [ "textarea"
      ; Printf.sprintf "%s %s" Menu.CSS.root Item_list.CSS.item ]
end

let action_of_event (e : #Dom_html.event Js.t) : action option =
  match Js.Opt.to_option e##.target with
  | None -> None
  | Some target ->
    let closest = Element.closest target (Printf.sprintf "[%s]" Attr.action) in
    match Js.Opt.to_option closest with
    | None -> None
    | Some e ->
      match Element.get_attribute e Attr.action with
      | None -> None
      | Some a -> Some (action_of_string a)

class t ?initial_focus_element (elt : Dom_html.element Js.t) () =
  object(self)
    val _container = find_element_by_class_exn elt CSS.container
    val _content = Element.query_selector elt Selector.content
    val _default_button = Element.query_selector elt Selector.default_button
    val mutable _buttons = Element.query_selector_all elt Selector.button
    val mutable _button_ripples = []
    val mutable _focus_trap : Focus_trap.t option = None
    val mutable _is_open = false
    val mutable _animation_thread = None
    val mutable _layout_thread = None
    val mutable _escape_key_action : action option = Some Close
    val mutable _scrim_click_action : action option = Some Close
    val mutable _auto_stack_buttons = true
    val mutable _are_buttons_stacked = false
    (* Event listeners. *)
    val mutable _click_listener = None
    val mutable _keydown_listener = None
    val mutable _doc_keydown_listener = None
    val mutable _resize_listener = None
    val mutable _orientation_change_listener = None

    inherit Widget.t elt () as super

    method! init () : unit =
      if super#has_class CSS.stacked
      then _auto_stack_buttons <- true;
      _button_ripples <- List.map Ripple.attach _buttons

    method! initial_sync_with_dom () : unit =
      let focus_trap =
        Focus_trap.make
          ?initial_focus:initial_focus_element
          ~click_outside_deactivates:true
          ~escape_deactivates:false
          _container in
      _focus_trap <- Some focus_trap;
      (* Attach event listeners. *)
      let click = Events.clicks super#root self#handle_interaction in
      let keydown = Events.keydowns super#root self#handle_interaction in
      _click_listener <- Some click;
      _keydown_listener <- Some keydown

    method! layout () : unit =
      Option.iter Lwt.cancel _layout_thread;
      super#layout ();
      let t =
        Animation.request ()
        >>= fun _ ->
        if _auto_stack_buttons
        then self#detect_stacked_buttons ();
        self#detect_scrollable_content ();
        Lwt.return () in
      _layout_thread <- Some t;
      Lwt.on_termination t (fun () -> _layout_thread <- None);
      Lwt.async (fun () -> t)

    method! destroy () : unit =
      super#destroy ();
      if _is_open then Lwt.ignore_result @@ self#close ~action:Destroy ();
      (match _animation_thread with
       | None -> ()
       | Some x ->
         Lwt.cancel x;
         self#handle_animation_timer_end ();
         _animation_thread <- None);
      (match _layout_thread with
       | None -> ()
       | Some x ->
         Lwt.cancel x;
         _layout_thread <- None);
      (* Detach event listeners. *)
      Option.iter Lwt.cancel _click_listener;
      Option.iter Lwt.cancel _keydown_listener;
      _click_listener <- None;
      _keydown_listener <- None;
      self#handle_closing ();
      (* Destroy internal components. *)
      List.iter Ripple.destroy _button_ripples;
      _button_ripples <- []

    method open_ () : unit Lwt.t =
      Option.iter Lwt.cancel _animation_thread;
      _is_open <- true;
      self#notify_opening ();
      self#handle_opening ();
      super#add_class CSS.opening;
      (* Wait a frame once display is no longer "none",
         to establish basis for animation. *)
      let t =
        Animation.request ()
        >>= fun _ -> Lwt_js.yield ()
        >>= fun () ->
        super#add_class CSS.open_;
        Element.add_class Dom_html.document##.body CSS.scroll_lock;
        Lwt_js.sleep Const.animation_open_time_s
        >>= fun () ->
        self#handle_animation_timer_end ();
        Option.iter Focus_trap.activate _focus_trap;
        self#layout ();
        self#notify_opened ();
        Lwt.return () in
      _animation_thread <- Some t;
      t

    method open_await () : action Lwt.t =
      self#open_ ()
      >>= fun () -> Events.make_event Event.closed super#root
      >>= fun e -> Lwt.return @@ Js.Opt.get e##.detail (fun () -> Close)

    method close ?(action = Close) () : action Lwt.t =
      match _is_open with
      | false -> Lwt.return action
      | true ->
        Option.iter Lwt.cancel _animation_thread;
        _is_open <- false;
        self#notify_closing action;
        self#handle_closing ();
        super#add_class CSS.closing;
        super#remove_class CSS.open_;
        Element.remove_class Dom_html.document##.body CSS.scroll_lock;
        let t =
          Lwt_js.sleep Const.animation_close_time_s
          >>= fun () ->
          Option.iter Focus_trap.deactivate _focus_trap;
          self#handle_animation_timer_end ();
          self#notify_closed action;
          Lwt.return () in
        _animation_thread <- Some t;
        t >>= (fun () -> Lwt.return action)

    method is_open : bool =
      _is_open

    method escape_action : action option =
      _escape_key_action

    method set_escape_action (x : action option) : unit =
      _escape_key_action <- x

    method scrim_click_action : action option =
      _scrim_click_action

    method set_scrim_click_action (x : action option) : unit =
      _scrim_click_action <- x

    method auto_stack_buttons : bool =
      _auto_stack_buttons

    method set_auto_stack_buttons (x : bool) : unit =
      _auto_stack_buttons <- x

    (* Private methods. *)

    method private handle_closing () : unit =
      Option.iter Lwt.cancel _resize_listener;
      Option.iter Lwt.cancel _orientation_change_listener;
      Option.iter Lwt.cancel _doc_keydown_listener;
      _resize_listener <- None;
      _orientation_change_listener <- None;
      _doc_keydown_listener <- None

    method private handle_opening () : unit =
      self#handle_closing ();
      let resize =
        Events.onresizes (fun _ _ -> self#layout (); Lwt.return_unit) in
      let orientation_change =
        Events.onorientationchanges (fun _ _ -> self#layout (); Lwt.return_unit) in
      let keydown =
        Events.keydowns Dom_html.document self#handle_document_keydown in
      _resize_listener <- Some resize;
      _orientation_change_listener <- Some orientation_change;
      _doc_keydown_listener <- Some keydown

    method private notify_closing (action : action) : unit =
      super#emit ~detail:action Event.closing

    method private notify_closed (action : action) : unit =
      super#emit ~detail:action Event.closed

    method private notify_opening () : unit =
      super#emit Event.opening

    method private notify_opened () : unit =
      super#emit Event.opened

    method private handle_interaction : 'a. (#Dom_html.event as 'a) Js.t ->
      unit Lwt.t -> unit Lwt.t =
      fun (e : #Dom_html.event Js.t) (_ : unit Lwt.t) ->
      let is_scrim =
        Js.Opt.map e##.target (fun target -> Element.matches target Selector.scrim)
        |> fun x -> Js.Opt.get x (fun () -> false) in
      let is_click = match Js.to_string e##._type with
        | "click" -> true | _ -> false in
      let is_enter =
        Js.Opt.case (Dom_html.CoerceTo.keyboardEvent e)
          (fun () -> false)
          (fun e -> match Dom_html.Keyboard_code.of_event e with
             | Enter -> true | _ -> false) in
      let is_space =
        Js.Opt.case (Dom_html.CoerceTo.keyboardEvent e)
          (fun () -> false)
          (fun e -> match Dom_html.Keyboard_code.of_event e with
             | Space -> true | _ -> false) in
      match is_click, is_scrim, _scrim_click_action with
      | true, true, Some action ->
        self#close ~action ()
        >>= (fun _ -> Lwt.return ())
      | _ ->
        if is_click || is_enter || is_space
        then (
          match action_of_event e with
          | Some action -> self#close ~action () >>= (fun _ -> Lwt.return ())
          | None ->
            let is_default =
              Js.Opt.map e##.target (fun target ->
                  Element.matches target Selector.suppress_default_press)
              |> (fun x -> Js.Opt.get x (fun () -> false))
              |> not in
            if is_enter && is_default
            then (
              match _default_button with
              | None -> Lwt.return_unit
              | Some x ->
                Js.Opt.case (Dom_html.CoerceTo.button x)
                  Lwt.return
                  (fun b -> b##click; Lwt.return_unit))
            else Lwt.return_unit)
        else Lwt.return_unit

    method private handle_document_keydown (e : Dom_html.keyboardEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      match Dom_html.Keyboard_code.of_event e, _escape_key_action with
      | Escape, Some x -> self#close ~action:x () >>= (fun _ -> Lwt.return ())
      | _ -> Lwt.return_unit

    method private handle_animation_timer_end () : unit =
      super#remove_class CSS.opening;
      super#remove_class CSS.closing

    method private detect_stacked_buttons () : unit =
      (* Remove the class first to let us measure the buttons' natural positions. *)
      super#remove_class CSS.stacked;
      let are_buttons_stacked = are_tops_misaligned _buttons in
      if are_buttons_stacked then super#add_class CSS.stacked;
      if not @@ Bool.equal are_buttons_stacked _are_buttons_stacked
      then (
        _buttons <- reverse_elements _buttons;
        _are_buttons_stacked <- are_buttons_stacked)

    method private detect_scrollable_content () : unit =
      (* Remove the class first to let us measure the natural height of the content. *)
      super#remove_class CSS.scrollable;
      match _content with
      | None -> ()
      | Some content ->
        if Element.is_scrollable content
        then super#add_class CSS.scrollable
  end

let make_element ?title ?content ?actions () : Dom_html.element Js.t =
  let title_id = match title with
    | None -> None
    | Some x -> Some (Js.to_string @@ (Tyxml_js.To_dom.of_element x)##.id) in
  let content_id = match content with
    | None -> None
    | Some x -> Some (Js.to_string @@ (Tyxml_js.To_dom.of_element x)##.id) in
  let scrim = Markup.create_scrim () in
  let actions = match actions with
    | None -> None
    | Some l -> Some (Markup.create_actions ~actions:l ()) in
  let surface = Markup.create_surface ?title ?content ?actions () in
  let container = Markup.create_container ~surface () in
  Tyxml_js.To_dom.of_element
  @@ Markup.create ?title_id ?content_id ~scrim ~container ()

let make ?title ?content ?actions () : t =
  let elt = make_element ?title ?content ?actions () in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
