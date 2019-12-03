open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Snackbar
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( >>= ) = Lwt.bind

type dismiss_reason =
  | Action
  | Dismiss
  | Timeout
  | Custom of string

module Const = struct
  let def_auto_dismiss_timeout_s = 5.

  let max_auto_dismiss_timeout_s = 10.

  let min_auto_dismiss_timeout_s = 4.

  (* These constants need to be kept in sync with the values in _variables.scss *)
  let animation_close_time_s = 0.075

  let animation_open_time_s = 0.15

  (** Number of seconds to wait between temporarily clearing the label
      text in the DOM and subsequently restoring it. This is necessary to force
      IE 11 to pick up the `aria-live` content change and announce it to the
      user *)
  let aria_live_delay_s = 1.
end

module Selector = struct
  let action = "." ^ CSS.action

  let dismiss = "." ^ CSS.dismiss

  let label = "." ^ CSS.label

  let surface = "." ^ CSS.surface
end

module Event = struct
  let closing : dismiss_reason Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_html.Event.make (CSS.root ^ ":closing")

  let close : dismiss_reason Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_html.Event.make (CSS.root ^ ":close")

  let opening : unit Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_html.Event.make (CSS.root ^ ":opening")

  let open_ : unit Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_html.Event.make (CSS.root ^ ":open")
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

let announce ?(label_elt : Element.t option) (aria_elt : Element.t) =
  let label_elt =
    match label_elt with
    | Some x -> x
    | None -> aria_elt
  in
  let live_attr = Js.string "aria-live" in
  let priority = aria_elt##getAttribute live_attr in
  let label_text =
    Js.Opt.map label_elt##.textContent (fun (s : Js.js_string Js.t) -> s##trim)
  in
  match Js.Opt.to_option label_text, Js.Opt.to_option priority with
  | None, _ | _, None -> Lwt.return_unit
  | Some label_text, Some priority ->
      (* Temporarily disable `aria-live` to prevent JAWS+Firefox from
        announcing the message twice. *)
      Element.set_attribute aria_elt "aria-live" "off";
      (* Temporarily clear `textContent` to force a DOM mutation event that will
        be detected by screen readers.
        `aria-live` elements are only announced when the element's `textContent`
        *changes*, so snackbars sent to the browser in the initial HTML response
        won't be read unless we clear the element's `textContent` first.
        Similarly, displaying the same snackbar message twice in a row doesn't
        trigger a DOM mutation event, so screen readers won't announce the second
        message unless we first clear `textContent`.
        We have to clear the label text two different ways to make it work in all
        browsers and screen readers:
        1. `textContent = ''` is required for IE11 + JAWS
        2. `innerHTML = '&nbsp;'` is required for Chrome + JAWS and NVDA
        All other browser/screen reader combinations support both methods.
        The wrapper `<span>` visually hides the space character so that it doesn't
        cause jank when added/removed.
        N.B.: Setting `position: absolute`, `opacity: 0`, or `height: 0` prevents
        Chrome from detecting the DOM change.
        This technique has been tested in:
        * JAWS 2019:
        - Chrome 70
        - Firefox 60 (ESR)
        - IE 11
        * NVDA 2018:
        - Chrome 70
        - Firefox 60 (ESR)
        - IE 11
        * ChromeVox 53 *)
      label_elt##.textContent := Js.some (Js.string "");
      label_elt##.innerHTML :=
        Js.string
          "<span style=\"display: inline-block; width: 0; height: 1px;\">&nbsp;</span>";
      (* Prevent visual jank by temporarily displaying the label text in the
        ::before pseudo-element. CSS generated content is normally announced
        by screen readers (except in IE 11;
        see https://tink.uk/accessibility-support-for-css-generated-content/);
        however, `aria-live` is turned off, so this DOM update will be ignored
        by screen readers. *)
      let attr = Js.string "data-mdc-snackbar-label-text" in
      label_elt##setAttribute attr label_text;
      Js_of_ocaml_lwt.Lwt_js.sleep Const.aria_live_delay_s
      >>= fun () ->
      (* Allow screen readers to announce changes to the DOM again. *)
      aria_elt##setAttribute live_attr priority;
      (* Remove the message from the ::before pseudo-element *)
      label_elt##removeAttribute attr;
      (* Restore the original label text,
            which will be announced by screen readers. *)
      label_elt##.textContent := Js.some label_text;
      Lwt.return_unit

class t
  ?(auto_dismiss_timeout = Const.def_auto_dismiss_timeout_s)
  ?(close_on_escape = true)
  elt
  () =
  object (self)
    val mutable animation_thread = Lwt.return_unit

    val mutable auto_dismiss_timer = None

    val mutable auto_dismiss_timeout = auto_dismiss_timeout

    val mutable close_on_escape = close_on_escape

    val mutable listeners = []

    val action_button : Element.t option = Element.query_selector elt Selector.action

    val label_element : Element.t = Element.query_selector_exn elt Selector.label

    val surface_element : Element.t = Element.query_selector_exn elt Selector.surface

    inherit Widget.t elt () as super

    method! initial_sync_with_dom () : unit =
      (* Attach event listeners *)
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ keydowns super#root self#handle_keydown
          ; clicks surface_element self#handle_surface_click ]
          @ listeners);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      (* Detach event listeners *)
      List.iter Lwt.cancel listeners;
      listeners <- [];
      super#destroy ()

    method timeout : float = auto_dismiss_timeout

    method set_timeout (x : float) : unit =
      if x <= Const.max_auto_dismiss_timeout_s && x >= Const.min_auto_dismiss_timeout_s
      then auto_dismiss_timeout <- x
      else
        let s =
          Printf.sprintf
            "timeout must be in the range (%g - %g), but got %g"
            Const.min_auto_dismiss_timeout_s
            Const.max_auto_dismiss_timeout_s
            x
        in
        failwith s

    method close_on_escape : bool = close_on_escape

    method set_close_on_escape (x : bool) : unit = close_on_escape <- x

    method label_text : string =
      Js.Opt.map label_element##.textContent Js.to_string
      |> fun x -> Js.Opt.get x (fun () -> "")

    method set_label_text (s : string) : unit =
      label_element##.textContent := Js.some @@ Js.string s

    method action_button_text : string option =
      match action_button with
      | None -> None
      | Some button -> Js.Opt.to_option @@ Js.Opt.map button##.textContent Js.to_string

    method set_action_button_text (s : string) : unit =
      match action_button with
      | None -> ()
      | Some button -> button##.textContent := Js.some @@ Js.string s

    method is_open : bool = super#has_class CSS.opening || super#has_class CSS.open_

    method open_ () : unit Lwt.t =
      Option.iter Lwt.cancel auto_dismiss_timer;
      auto_dismiss_timer <- None;
      self#notify_opening ();
      super#remove_class CSS.closing;
      super#add_class CSS.opening;
      (* FIXME check this fun, seems it is broken *)
      (* announce _label_element; *)
      (* Wait a frame once display is no longe "none",
       to establish basis for animation *)
      Lwt.cancel animation_thread;
      animation_thread <-
        (Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame ()
        >>= Js_of_ocaml_lwt.Lwt_js.yield
        >>= fun () ->
        super#add_class CSS.open_;
        Js_of_ocaml_lwt.Lwt_js.sleep Const.animation_open_time_s
        >>= fun () ->
        super#remove_class CSS.opening;
        self#notify_opened ();
        Lwt.return ());
      let dismiss_timer =
        animation_thread
        >>= fun () ->
        Js_of_ocaml_lwt.Lwt_js.sleep self#timeout >>= self#close ~reason:Timeout
      in
      auto_dismiss_timer <- Some dismiss_timer;
      animation_thread

    method open_await () : dismiss_reason Lwt.t =
      self#open_ ()
      >>= fun () ->
      Js_of_ocaml_lwt.Lwt_js_events.make_event Event.close super#root
      >>= fun e -> Lwt.return @@ Widget.event_detail e

    method close ?(reason = Dismiss) () : dismiss_reason Lwt.t =
      match self#is_open with
      | false -> Lwt.return reason
      | true ->
          Lwt.cancel animation_thread;
          self#clear_auto_dismiss_timer ();
          self#notify_closing reason;
          super#add_class CSS.closing;
          super#remove_class CSS.open_;
          super#remove_class CSS.opening;
          animation_thread <-
            (Js_of_ocaml_lwt.Lwt_js.sleep Const.animation_close_time_s
            >>= fun () ->
            super#remove_class CSS.closing;
            self#notify_closed reason;
            Lwt.return ());
          animation_thread >>= fun () -> Lwt.return reason

    method private notify_opening () : unit = super#emit Event.opening

    method private notify_opened () : unit = super#emit Event.open_

    method private notify_closing (reason : dismiss_reason) : unit =
      super#emit ~detail:reason Event.closing

    method private notify_closed (reason : dismiss_reason) : unit =
      super#emit ~detail:reason Event.close

    method private handle_surface_click
        (e : #Dom_html.event Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      Js.Opt.map e##.target (fun (elt : Dom_html.element Js.t) ->
          if Js.Opt.test @@ Element.closest elt Selector.action
          then self#handle_action_button_click ()
          else if Js.Opt.test @@ Element.closest elt Selector.dismiss
          then self#handle_action_icon_click ()
          else Lwt.return_unit)
      |> fun x -> Js.Opt.get x Lwt.return

    method private handle_keydown
        (e : Dom_html.keyboardEvent Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      match Dom_html.Keyboard_code.of_event e, self#close_on_escape with
      | Escape, true -> self#close ~reason:Dismiss () >>= fun _ -> Lwt.return_unit
      | _ -> Lwt.return_unit

    method private handle_action_button_click () : unit Lwt.t =
      self#close ~reason:Action () >>= fun _ -> Lwt.return_unit

    method private handle_action_icon_click () : unit Lwt.t =
      self#close ~reason:Dismiss () >>= fun _ -> Lwt.return_unit

    method private clear_auto_dismiss_timer () =
      Option.iter Lwt.cancel auto_dismiss_timer;
      auto_dismiss_timer <- None
  end

let attach ?auto_dismiss_timeout ?close_on_escape (elt : #Dom_html.element Js.t) : t =
  new t ?auto_dismiss_timeout ?close_on_escape (Element.coerce elt) ()

let make
    ?classes
    ?a
    ?leading
    ?stacked
    ?dismiss
    ?action
    ?actions
    ?label
    ?surface
    ?children
    ?auto_dismiss_timeout
    ?close_on_escape
    () =
  D.snackbar
    ?classes
    ?a
    ?leading
    ?stacked
    ?dismiss
    ?action
    ?actions
    ?label
    ?surface
    ?children
    ()
  |> Tyxml_js.To_dom.of_div
  |> attach ?auto_dismiss_timeout ?close_on_escape
