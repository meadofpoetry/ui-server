open Js_of_ocaml
include Components_tyxml.Dialog
module Markup_js =
  Components_tyxml.Dialog.Make
    (Js_of_ocaml_tyxml.Tyxml_js.Xml)
    (Js_of_ocaml_tyxml.Tyxml_js.Svg)
    (Js_of_ocaml_tyxml.Tyxml_js.Html)

let ( >>= ) = Lwt.bind

let name = "dialog"

let are_tops_misaligned (elts : Dom_html.element Js.t list) : bool =
  List.map (fun (e : Dom_html.element Js.t) -> e##.offsetTop) elts
  |> List.sort_uniq compare
  |> function
  | [] | [_] -> false
  | _ -> true

let reverse_elements (elts : Dom_html.element Js.t list) : Dom_html.element Js.t list =
  let lst = List.rev elts in
  List.iter
    (fun (elt : Dom_html.element Js.t) ->
      let parent = Element.get_parent elt in
      Js.Opt.iter parent (fun parent -> Element.append_child parent elt))
    lst;
  lst

module Event = struct
  let opening : unit Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_html.Event.make (name ^ ":opening")

  let open_ : unit Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_html.Event.make (name ^ ":open")

  let closing : action Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_html.Event.make (name ^ ":closing")

  let close : action Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_html.Event.make (name ^ ":close")
end

module Lwt_js_events = struct
  let opening ?use_capture ?passive t =
    Js_of_ocaml_lwt.Lwt_js_events.make_event ?use_capture ?passive Event.opening t

  let openings ?cancel_handler ?use_capture ?passive t =
    Js_of_ocaml_lwt.Lwt_js_events.seq_loop
      ?cancel_handler
      ?use_capture
      ?passive
      opening
      t

  let open_ ?use_capture ?passive t =
    Js_of_ocaml_lwt.Lwt_js_events.make_event ?use_capture ?passive Event.open_ t

  let opens ?cancel_handler ?use_capture ?passive t =
    Js_of_ocaml_lwt.Lwt_js_events.seq_loop ?cancel_handler ?use_capture ?passive open_ t

  let closing ?use_capture ?passive t =
    Js_of_ocaml_lwt.Lwt_js_events.make_event ?use_capture ?passive Event.closing t

  let closings ?cancel_handler ?use_capture ?passive t =
    Js_of_ocaml_lwt.Lwt_js_events.seq_loop
      ?cancel_handler
      ?use_capture
      ?passive
      closing
      t

  let close ?use_capture ?passive t =
    Js_of_ocaml_lwt.Lwt_js_events.make_event ?use_capture ?passive Event.close t

  let closes ?cancel_handler ?use_capture ?passive t =
    Js_of_ocaml_lwt.Lwt_js_events.seq_loop ?cancel_handler ?use_capture ?passive close t
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

  let container = "." ^ CSS.container

  let content = "." ^ CSS.content

  let button = "." ^ CSS.button

  let default_button = "." ^ CSS.button_default

  let suppress_default_press =
    String.concat
      ", "
      ["textarea"; Printf.sprintf "%s %s" Menu.CSS.root Item_list.CSS.item]
end

let action_of_event (e : #Dom_html.event Js.t) : action option =
  match Js.Opt.to_option e##.target with
  | None -> None
  | Some target -> (
      let closest = Element.closest target (Printf.sprintf "[%s]" Attr.action) in
      match Js.Opt.to_option closest with
      | None -> None
      | Some e -> (
        match Element.get_attribute e Attr.action with
        | None -> None
        | Some a -> Some (action_of_string a)))

class t ?initial_focus_element (elt : Dom_html.element Js.t) () =
  object (self)
    val _container = Element.query_selector_exn elt Selector.container

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
    val mutable _listeners = []

    val mutable _temp_listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      if super#has_class CSS.stacked then _auto_stack_buttons <- true;
      _button_ripples <- List.map Ripple.attach _buttons

    method! initial_sync_with_dom () : unit =
      let focus_trap =
        Focus_trap.make
          ?initial_focus:initial_focus_element
          ~click_outside_deactivates:true
          ~escape_deactivates:false
          _container
      in
      _focus_trap <- Some focus_trap;
      (* Attach event listeners. *)
      _listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ clicks super#root self#handle_interaction
          ; keydowns super#root self#handle_interaction ]
          @ _listeners)

    method! layout () : unit =
      Option.iter Lwt.cancel _layout_thread;
      super#layout ();
      let t =
        Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame ()
        >>= fun () ->
        if _auto_stack_buttons then self#detect_stacked_buttons ();
        self#detect_scrollable_content ();
        Lwt.return ()
      in
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
      List.iter Lwt.cancel _listeners;
      _listeners <- [];
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
        Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame ()
        >>= fun () ->
        Js_of_ocaml_lwt.Lwt_js.yield ()
        >>= fun () ->
        super#add_class CSS.open_;
        Element.add_class Dom_html.document##.body CSS.scroll_lock;
        Js_of_ocaml_lwt.Lwt_js.sleep Const.animation_open_time_s
        >>= fun () ->
        self#handle_animation_timer_end ();
        Option.iter Focus_trap.activate _focus_trap;
        self#layout ();
        self#notify_opened ();
        Lwt.return ()
      in
      _animation_thread <- Some t;
      t

    method open_await () : action Lwt.t =
      self#open_ ()
      >>= fun () ->
      Lwt_js_events.close super#root
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
            Js_of_ocaml_lwt.Lwt_js.sleep Const.animation_close_time_s
            >>= fun () ->
            Option.iter Focus_trap.deactivate _focus_trap;
            self#handle_animation_timer_end ();
            self#notify_closed action;
            Lwt.return ()
          in
          _animation_thread <- Some t;
          t >>= fun () -> Lwt.return action

    method is_open : bool = _is_open

    method escape_action : action option = _escape_key_action

    method set_escape_action (x : action option) : unit = _escape_key_action <- x

    method scrim_click_action : action option = _scrim_click_action

    method set_scrim_click_action (x : action option) : unit = _scrim_click_action <- x

    method auto_stack_buttons : bool = _auto_stack_buttons

    method set_auto_stack_buttons (x : bool) : unit = _auto_stack_buttons <- x

    method content : Dom_html.element Js.t option = _content

    method append_to_body ?(body = Dom_html.document##.body) () : unit =
      Dom.appendChild body super#root

    method remove_from_dom () : unit =
      Js.Opt.iter (Element.get_parent super#root) (fun parent ->
          Dom.removeChild parent super#root)

    (* Private methods. *)
    method private handle_closing () : unit =
      List.iter Lwt.cancel _temp_listeners;
      _temp_listeners <- []

    method private handle_opening () : unit =
      self#handle_closing ();
      _temp_listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.
          [ onresizes (fun _ _ ->
                self#layout ();
                Lwt.return_unit)
          ; onorientationchanges (fun _ _ ->
                self#layout ();
                Lwt.return_unit)
          ; keydowns Dom_html.document self#handle_document_keydown ]

    method private notify_closing (action : action) : unit =
      super#emit ~detail:action Event.closing

    method private notify_closed (action : action) : unit =
      super#emit ~detail:action Event.close

    method private notify_opening () : unit = super#emit Event.opening

    method private notify_opened () : unit = super#emit Event.open_

    method private handle_interaction
        : 'a. (#Dom_html.event as 'a) Js.t -> unit Lwt.t -> unit Lwt.t =
      fun (e : #Dom_html.event Js.t) (_ : unit Lwt.t) ->
        let target = Dom.eventTarget e in
        let is_scrim = Element.matches target Selector.scrim in
        let is_click =
          match Js.to_string e##._type with
          | "click" -> true
          | _ -> false
        in
        let is_enter =
          Js.Opt.case
            (Dom_html.CoerceTo.keyboardEvent e)
            (fun () -> false)
            (fun e ->
              match Dom_html.Keyboard_code.of_event e with
              | Enter -> true
              | _ -> false)
        in
        let is_space =
          Js.Opt.case
            (Dom_html.CoerceTo.keyboardEvent e)
            (fun () -> false)
            (fun e ->
              match Dom_html.Keyboard_code.of_event e with
              | Space -> true
              | _ -> false)
        in
        match is_click, is_scrim, _scrim_click_action with
        | true, true, Some action -> self#close ~action () >>= fun _ -> Lwt.return ()
        | _ ->
            if is_click || is_enter || is_space
            then
              match action_of_event e with
              | Some action -> self#close ~action () >>= fun _ -> Lwt.return ()
              | None ->
                  let is_default =
                    not @@ Element.matches target Selector.suppress_default_press
                  in
                  if is_enter && is_default
                  then
                    match _default_button with
                    | None -> Lwt.return_unit
                    | Some x ->
                        Js.Opt.case (Dom_html.CoerceTo.button x) Lwt.return (fun b ->
                            b##click;
                            Lwt.return_unit)
                  else Lwt.return_unit
            else Lwt.return_unit

    method private handle_document_keydown
        (e : Dom_html.keyboardEvent Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      match Dom_html.Keyboard_code.of_event e, _escape_key_action with
      | Escape, Some x -> self#close ~action:x () >>= fun _ -> Lwt.return ()
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
          if Element.is_scrollable content then super#add_class CSS.scrollable
  end

let make ?classes ?attrs ?title ?content ?actions ?container ?scrim () : t =
  let title_id =
    match title with
    | None -> None
    | Some x ->
        let elt = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element x in
        Some (Js.to_string @@ elt##.id)
  in
  let content_id =
    match content with
    | None -> None
    | Some x ->
        let elt = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element x in
        Some (Js.to_string @@ elt##.id)
  in
  let elt =
    Js_of_ocaml_tyxml.(
      Tyxml_js.To_dom.of_element
      @@ Markup_js.create
           ?classes
           ?attrs
           ?title_id
           ?content_id
           ?container
           ?scrim
           ?title
           ?content
           ?actions
           ())
  in
  new t elt ()

let attach ?initial_focus_element (elt : #Dom_html.element Js.t) : t =
  new t ?initial_focus_element (Element.coerce elt) ()
