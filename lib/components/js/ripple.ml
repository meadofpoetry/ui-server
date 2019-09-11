open Js_of_ocaml
open Js_of_ocaml_lwt
include Components_tyxml.Ripple

let ( >>= ) = Lwt.bind

type frame =
  { width : float
  ; height : float }

type point =
  { x : float
  ; y : float }

type activation_state =
  { is_activated : bool
  ; has_deactivation_ux_run : bool
  ; was_activated_by_pointer : bool
  ; was_element_made_active : bool
  ; activation_event : Dom_html.event Js.t option
  ; is_programmatic : bool }

let default_activation_state =
  { is_activated = false
  ; has_deactivation_ux_run = false
  ; was_activated_by_pointer = false
  ; was_element_made_active = false
  ; activation_event = None
  ; is_programmatic = false }

module Util = struct
  let px = Printf.sprintf "%dpx"

  let supports_passive : bool option ref = ref None

  let suppots_css_variables_ : bool option ref = ref None

  (* FIXME implement *)
  let apply_passive ?(global_obj = Dom_html.window) ?(force_refresh = false) () =
    ignore global_obj;
    match !supports_passive, force_refresh with
    | None, _ | _, true -> ( try () with _ -> ())
    | _ -> ()

  let detect_edge_pseudo_var_bug (window : Dom_html.window Js.t) : bool =
    let doc = window##.document in
    let node = doc##createElement (Js.string "div") in
    node##.className := Js.string "mdc-ripple-surface--test-edge-var-bug";
    ignore (doc##.body##appendChild (node :> Dom.node Js.t));
    let (computed_style : Dom_html.cssStyleDeclaration Js.t Js.opt) =
      (Js.Unsafe.coerce window)##getComputedStyle node
    in
    let has_pseudo_var_bug =
      match Js.Opt.to_option computed_style with
      | None -> true
      | Some s -> String.equal "solid" (Js.to_string s##.borderTopStyle)
    in
    ignore @@ doc##.body##removeChild (node :> Dom.node Js.t);
    has_pseudo_var_bug

  let supports_css_variables ?(force_refresh = false) (window : Dom_html.window Js.t) :
      bool =
    match force_refresh, !suppots_css_variables_ with
    | false, Some x -> x
    | true, _ | _, None ->
        let window = Js.Unsafe.coerce window in
        let supports_function_present =
          if not @@ Js.Optdef.test window ##. CSS
          then false
          else
            Js.Optdef.test window ##. CSS##.supports
            && String.equal
                 "function"
                 (Js.to_string (Js.typeof window ##. CSS##.supports))
        in
        if not supports_function_present
        then false
        else
          let explicitly_supports_css_vars =
            window ##. CSS##supports (Js.string "--css-vars") (Js.string "yes")
            |> Js.to_bool
          in
          let we_are_feature_detecting_safary_10_plus =
            let a =
              Js.to_bool @@ window ##. CSS##supports (Js.string "(--css-vars: yes)")
            in
            let b =
              Js.to_bool
              @@ window ##. CSS##supports (Js.string "color") (Js.string "#00000000")
            in
            a && b
          in
          let supports =
            if explicitly_supports_css_vars || we_are_feature_detecting_safary_10_plus
            then not @@ detect_edge_pseudo_var_bug window
            else false
          in
          if force_refresh then suppots_css_variables_ := Some supports;
          supports

  let get_normalized_event_coords
      (event : Dom_html.event Js.t)
      (page_offset : point)
      (client_rect : Dom_html.clientRect Js.t) =
    let {x; y} = page_offset in
    let doc_x = x +. client_rect##.left in
    let doc_y = y +. client_rect##.top in
    let normalized_x, normalized_y =
      match Js.to_string event##._type with
      | "touchstart" ->
          let (ev : Dom_html.touchEvent Js.t) = Js.Unsafe.coerce event in
          let (touch : Dom_html.touch Js.t) =
            Js.Optdef.get (ev##.changedTouches##item 0) (fun () -> assert false)
          in
          float_of_int touch##.pageX -. doc_x, float_of_int touch##.pageY -. doc_y
      | _ ->
          let (ev : Dom_html.mouseEvent Js.t) = Js.Unsafe.coerce event in
          let page_x = Js.Optdef.case ev##.pageX (fun () -> 0.) float_of_int in
          let page_y = Js.Optdef.case ev##.pageY (fun () -> 0.) float_of_int in
          page_x -. doc_x, page_y -. doc_y
    in
    {x = normalized_x; y = normalized_y}
end

type adapter =
  { event_target : Dom_html.element Js.t
  ; style_target : Dom_html.element Js.t
  ; is_unbounded : unit -> bool
  ; is_surface_active : unit -> bool
  ; is_surface_disabled : unit -> bool
  ; contains_event_target : Dom_html.element Js.t -> bool
  ; compute_bounding_rect : unit -> Dom_html.clientRect Js.t }

let update_css_variable node name value =
  (Js.Unsafe.coerce node##.style)##setProperty
    (Js.string name)
    (match value with
    | None -> Js.string ""
    | Some s -> Js.string s)

let make_default_adapter ?(is_unbounded = false) (elt : #Dom_html.element Js.t) : adapter
    =
  let elt = Element.coerce elt in
  { event_target = elt
  ; style_target = elt
  ; is_unbounded = (fun () -> is_unbounded)
  ; is_surface_active = (fun () -> Element.matches elt ":active")
  ; is_surface_disabled =
      (fun () ->
        if Js.Optdef.test (Js.Unsafe.coerce elt)##.disabled
        then Js.to_bool (Js.Unsafe.coerce elt)##.disabled
        else false)
  ; contains_event_target = Element.contains elt
  ; compute_bounding_rect = (fun () -> elt##getBoundingClientRect) }

module Const = struct
  let padding = 20

  let initial_origin_scale = 0.6

  let deactivation_timeout_s = 0.225

  let fg_deactivation_s = 0.15

  let tap_delay_s = 0.3
end

type activation_event =
  | Touchstart of Dom_html.touchEvent Js.t
  | Pointerdown of Dom_html.event Js.t
  | Mousedown of Dom_html.mouseEvent Js.t
  | Keydown of Dom_html.keyboardEvent Js.t

type deactivation_event =
  | Touchend of Dom_html.touchEvent Js.t
  | Pointerup of Dom_html.event Js.t
  | Mouseup of Dom_html.mouseEvent Js.t

let (pointer_deactivation_event_types : Dom_html.event Js.t Dom_html.Event.typ list) =
  Dom_html.Event.[make "touchend"; make "pointerup"; make "mouseup"]

class t (adapter : adapter) () =
  object (self)
    val mutable _layout_frame = None

    val mutable _frame = {width = 0.; height = 0.}

    val mutable _activation_state = default_activation_state

    val mutable _initial_size = 0.

    val mutable _max_radius = 0.

    val mutable _fg_scale = 0.

    val mutable _unbounded_coords = {x = 0.; y = 0.}

    val mutable _activation_thread = None

    val mutable _fg_deactivation_thread = None

    val mutable _activation_animation_has_ended = false

    val mutable _previous_activation_event : Dom_html.event Js.t option = None

    val mutable _activated_targets = []

    (* DOM Events *)
    val mutable listeners = []

    val mutable deactivation_listeners = []

    method private add_class (c : string) : unit =
      Element.add_class adapter.style_target c

    method private remove_class (c : string) : unit =
      Element.remove_class adapter.style_target c

    method private update_css_var (var : string) (value : string option) =
      update_css_variable adapter.style_target var value

    method init () : unit =
      let supports_press_ripple = self#supports_press_ripple () in
      self#register_root_handlers supports_press_ripple;
      if supports_press_ripple
      then
        let t =
          Lwt_js_events.request_animation_frame ()
          >>= fun () ->
          self#add_class CSS.root;
          if adapter.is_unbounded ()
          then (
            self#add_class CSS.unbounded;
            self#layout_internal ());
          Lwt.return ()
        in
        Lwt.ignore_result t

    method destroy () =
      if self#supports_press_ripple ()
      then (
        (match _activation_thread with
        | None -> ()
        | Some x ->
            Lwt.cancel x;
            _activation_thread <- None;
            self#remove_class CSS.fg_activation);
        (match _fg_deactivation_thread with
        | None -> ()
        | Some x ->
            Lwt.cancel x;
            _fg_deactivation_thread <- None;
            self#remove_class CSS.fg_deactivation);
        let t =
          Lwt_js_events.request_animation_frame ()
          >>= fun () ->
          self#remove_class CSS.root;
          self#remove_class CSS.unbounded;
          self#remove_css_vars ();
          Lwt.return ()
        in
        Lwt.ignore_result t)
      else (
        self#deregister_root_handlers ();
        self#deregister_deactivation_handlers ())

    method layout () : unit =
      Option.iter Lwt.cancel _layout_frame;
      let t =
        Lwt_js_events.request_animation_frame ()
        >>= fun () ->
        self#layout_internal ();
        Lwt.return ()
      in
      Lwt.on_termination t (fun () -> _layout_frame <- None);
      _layout_frame <- Some t;
      Lwt.async (fun () -> Lwt.catch (fun () -> t) (fun _ -> Lwt.return_unit))

    method unbounded : bool = adapter.is_unbounded ()

    method set_unbounded (x : bool) : unit =
      if x then self#add_class CSS.unbounded else self#remove_class CSS.unbounded

    method activate ?(event : Dom_html.event Js.t option) () : unit Lwt.t =
      self#activate_ event
    (** [event] - optional event containing position information. *)

    method deactivate () : unit Lwt.t = self#deactivate_ ()

    (* Private methods *)
    method private handle_focus (_ : Dom_html.event Js.t) (_ : unit Lwt.t) : unit Lwt.t =
      Lwt_js_events.request_animation_frame ()
      >>= fun _ -> Lwt.return @@ self#add_class CSS.bg_focused

    method private handle_blur (_ : Dom_html.event Js.t) (_ : unit Lwt.t) : unit Lwt.t =
      Lwt_js_events.request_animation_frame ()
      >>= fun () -> Lwt.return @@ self#remove_class CSS.bg_focused

    method private supports_press_ripple () : bool =
      Util.supports_css_variables Dom_html.window
    (** We compute this property so that we are not querying information about
        the client until the point in time where the foundation requests it.
        This prevents scenarios where client-side feature-detection may happen
        too early, such as when components are rendered on the server
        and then initialized at mount time on the client. *)

    method private register_root_handlers (supports_press_ripple : bool) : unit =
      let elt = adapter.event_target in
      Lwt_js_events.(
        if supports_press_ripple
        then
          let handler e _ = self#activate ~event:(e :> Dom_html.event Js.t) () in
          (* ;
           * if adapter.is_unbounded () then (
           *   _on_resize <- onresizes (fun _ _ -> self#layout (); Lwt.return_unit)) *)
          listeners <-
            [ touchstarts ~passive:true elt handler
            ; pointerdowns elt handler
            ; mousedowns elt handler
            ; keydowns elt handler
            ; focuses elt self#handle_focus
            ; blurs elt self#handle_blur ]
            @ listeners)

    method private deregister_root_handlers () : unit =
      List.iter Lwt.cancel listeners;
      listeners <- []

    method register_deactivation_handlers (event : Dom_html.event Js.t) =
      let handler _ _ = self#deactivate () in
      match Js.to_string event##._type with
      | "keydown" ->
          let listener = Lwt_js_events.keyups adapter.event_target handler in
          deactivation_listeners <- listener :: deactivation_listeners
      | _ ->
          pointer_deactivation_event_types
          |> List.map (fun x ->
                 let e = Lwt_js_events.make_event x in
                 Lwt_js_events.seq_loop e adapter.event_target handler)
          |> fun l -> deactivation_listeners <- deactivation_listeners @ l

    method private deregister_deactivation_handlers () : unit =
      List.iter Lwt.cancel deactivation_listeners;
      deactivation_listeners <- []

    method private remove_css_vars () : unit =
      List.iter (fun v -> self#update_css_var v None) CSS.Var.vars

    method activate_ (event : Dom_html.event Js.t option) : unit Lwt.t =
      let is_same_interaction () =
        match _previous_activation_event, event with
        | None, _ | _, None -> false
        | Some event, Some e ->
            let typ = Js.to_string event##._type in
            let prev_typ = Js.to_string e##._type in
            not @@ String.equal typ prev_typ
      in
      let has_activated_child () =
        match event with
        | None -> false
        | Some _ ->
            List.find_opt adapter.contains_event_target _activated_targets
            |> Option.is_some
      in
      if adapter.is_surface_disabled ()
      then Lwt.return ()
      else if _activation_state.is_activated
      then Lwt.return ()
      else if is_same_interaction ()
      then Lwt.return ()
      else if has_activated_child ()
      then self#reset_activation_state ()
      else (
        Option.iter
          (fun e ->
            Js.Opt.iter e##.target (fun x ->
                _activated_targets <- x :: _activated_targets);
            self#register_deactivation_handlers e)
          event;
        let is_activated = true in
        let is_programmatic = Option.is_none event in
        let activation_event = event in
        let was_activated_by_pointer =
          match event with
          | None -> false
          | Some e ->
              let typ = Js.to_string e##._type in
              let lst = ["mousedown"; "touchstart"; "pointerdown"] in
              List.exists (String.equal typ) lst
        in
        let was_element_made_active = self#check_element_made_active event in
        let state =
          { _activation_state with
            is_activated
          ; is_programmatic
          ; activation_event
          ; was_element_made_active
          ; was_activated_by_pointer }
        in
        _activation_state <- state;
        if was_element_made_active
        then _activation_thread <- Some (self#animate_activation ());
        Lwt_js_events.request_animation_frame ()
        >>= fun () ->
        (* Reset array on next frame after the current event has had a chance to
           bubble to prevent ancestor ripples. *)
        _activated_targets <- [];
        if (not was_element_made_active)
           &&
           match event with
           | None -> false
           | Some (e : Dom_html.event Js.t) ->
               Js.Opt.case
                 (Dom_html.CoerceTo.keyboardEvent e)
                 (fun () -> false)
                 (fun e ->
                   match Dom_html.Keyboard_code.of_event e with
                   | Space -> true
                   | _ -> false)
        then (
          let was_element_made_active = self#check_element_made_active event in
          if was_element_made_active then Lwt.ignore_result @@ self#animate_activation ();
          _activation_state <- {_activation_state with was_element_made_active});
        (* Reset activation state immediately if element was not made active. *)
        if not _activation_state.was_element_made_active
        then _activation_state <- default_activation_state;
        Lwt.return ())

    method check_element_made_active (event : Dom_html.event Js.t option) : bool =
      match event with
      | None -> true
      | Some e -> (
        match Js.to_string e##._type with
        | "keydown" -> adapter.is_surface_active ()
        | _ -> true)

    method private animate_activation () : unit Lwt.t =
      self#layout_internal ();
      let translate_start, translate_end =
        if self#unbounded
        then "", ""
        else
          let sp, ep = self#get_fg_translation_coordinates () in
          Printf.sprintf "%gpx, %gpx" sp.x sp.y, Printf.sprintf "%gpx, %gpx" ep.x ep.y
      in
      self#update_css_var CSS.Var.fg_translate_start (Some translate_start);
      self#update_css_var CSS.Var.fg_translate_end (Some translate_end);
      (* Cancel any ongoing activation/deactivation animations *)
      Option.iter Lwt.cancel _activation_thread;
      _activation_thread <- None;
      Option.iter Lwt.cancel _fg_deactivation_thread;
      _fg_deactivation_thread <- None;
      self#rm_bounded_activation_classes ();
      self#remove_class CSS.fg_deactivation;
      (* Force layout in order to re-trigger the animation. *)
      ignore @@ adapter.compute_bounding_rect ();
      self#add_class CSS.fg_activation;
      Lwt.catch
        (fun () ->
          Lwt_js.sleep Const.deactivation_timeout_s
          >>= fun () ->
          _activation_animation_has_ended <- true;
          self#run_deactivation_ux_logic_if_ready ())
        (function
          | Lwt.Canceled -> Lwt.return_unit
          | exn -> Lwt.fail exn)

    method private get_fg_translation_coordinates () : point * point =
      let start_point =
        match _activation_state.was_activated_by_pointer with
        | true ->
            let window_page_offset =
              { x = (Js.Unsafe.coerce Dom_html.window)##.pageXOffset
              ; y = (Js.Unsafe.coerce Dom_html.window)##.pageYOffset }
            in
            Util.get_normalized_event_coords
              (match _activation_state.activation_event with
              | None -> raise Not_found
              | Some x -> x)
              window_page_offset
              (adapter.compute_bounding_rect ())
        | false -> {x = _frame.width /. 2.; y = _frame.height /. 2.}
      in
      (* Center the element around the start point. *)
      let start_point =
        { x = start_point.x -. (_initial_size /. 2.)
        ; y = start_point.y -. (_initial_size /. 2.) }
      in
      let end_point =
        { x = (_frame.width /. 2.) -. (_initial_size /. 2.)
        ; y = (_frame.height /. 2.) -. (_initial_size /. 2.) }
      in
      start_point, end_point

    method private run_deactivation_ux_logic_if_ready () : unit Lwt.t =
      (* This method is called both when a pointing device is released,
       * and when the activation animation ends.
       * The deactivation animation should only run after both of those occur. *)
      let {has_deactivation_ux_run; is_activated; _} = _activation_state in
      let activation_has_ended = has_deactivation_ux_run || not is_activated in
      if activation_has_ended && _activation_animation_has_ended
      then (
        self#rm_bounded_activation_classes ();
        self#add_class CSS.fg_deactivation;
        let t =
          Lwt_js.sleep Const.fg_deactivation_s
          >>= fun () -> Lwt.return @@ self#remove_class CSS.fg_deactivation
        in
        _fg_deactivation_thread <- Some t;
        Lwt.on_success t (fun () -> _fg_deactivation_thread <- None);
        t)
      else Lwt.return ()

    method private rm_bounded_activation_classes () : unit =
      self#remove_class CSS.fg_activation;
      _activation_animation_has_ended <- false;
      ignore @@ adapter.compute_bounding_rect ()

    method private reset_activation_state () : unit Lwt.t =
      _previous_activation_event <- _activation_state.activation_event;
      _activation_state <- default_activation_state;
      (* Touch devices may fire additional events for the
         same interaction within a short time. Store the previous
         event until it's safe to assume that subsequent events are
         for new interactions. *)
      Lwt_js.sleep Const.tap_delay_s
      >>= fun () ->
      _previous_activation_event <- None;
      Lwt.return ()

    method private deactivate_ () : unit Lwt.t =
      let ({is_activated; is_programmatic; _} as state) = _activation_state in
      match is_activated, is_programmatic with
      (* This can happen in scenarios such as when you have a keyup event
         that blurs the element. *)
      | false, _ -> Lwt.return ()
      | _, true ->
          Lwt.async (fun () ->
              Lwt_js_events.request_animation_frame ()
              >>= fun () -> self#animate_deactivation state);
          self#reset_activation_state ()
      | _, false ->
          self#deregister_deactivation_handlers ();
          Lwt_js_events.request_animation_frame ()
          >>= fun () ->
          let state = {state with has_deactivation_ux_run = true} in
          _activation_state <- state;
          self#animate_deactivation state >>= self#reset_activation_state

    method private animate_deactivation (a : activation_state) : unit Lwt.t =
      if a.was_activated_by_pointer || a.was_element_made_active
      then self#run_deactivation_ux_logic_if_ready ()
      else Lwt.return ()

    method private layout_internal () : unit =
      let rect = adapter.compute_bounding_rect () in
      let width = Js.Optdef.get rect##.width (fun () -> raise Not_found) in
      let height = Js.Optdef.get rect##.height (fun () -> raise Not_found) in
      _frame <- {width; height};
      let max_dim = Float.max width height in
      (* Surface diameter is treated differently for unbounded vs. bounded ripples.
         Unbounded ripple diameter is calculated smaller since the surface is expected
         to already be padded appropriately to extend the hitbox, and the ripple is
         expected to meet the edges of the padded hitbox (which is typically square).
         Bounded ripples, on the other hand, are fully expected to expand beyond the
         surface's longest diameter (calculated based on the diagonal plus a constant
         padding), and are clipped at the surface's border via `overflow: hidden`. *)
      let max_radius =
        match self#unbounded with
        | true -> max_dim
        | false ->
            let hypotenuse = sqrt ((width *. width) +. (height *. height)) in
            hypotenuse +. float_of_int Const.padding
      in
      (*  Ripple is sized as a fraction of the largest dimension of the surface,
          then scales up using a CSS scale transform. *)
      _max_radius <- max_radius;
      _initial_size <- floor @@ (max_dim *. Const.initial_origin_scale);
      _fg_scale <- max_radius /. _initial_size;
      self#update_layout_css_vars ()

    method private update_layout_css_vars () : unit =
      let fg_size = Some (Util.px @@ int_of_float _initial_size) in
      self#update_css_var CSS.Var.fg_size fg_size;
      let fg_scale = Some (Printf.sprintf "%g" _fg_scale) in
      self#update_css_var CSS.Var.fg_scale fg_scale;
      if self#unbounded
      then (
        let x = Float.(round ((_frame.width /. 2.) -. (_initial_size /. 2.))) in
        let y = Float.(round ((_frame.height /. 2.) -. (_initial_size /. 2.))) in
        _unbounded_coords <- {x; y};
        self#update_css_var CSS.Var.left (Some (Printf.sprintf "%gpx" x));
        self#update_css_var CSS.Var.top (Some (Printf.sprintf "%gpx" y)))

    initializer
    self#set_unbounded @@ adapter.is_unbounded ();
    self#init ()
  end

let attach ?unbounded (elt : #Dom_html.element Js.t) : t =
  let adapter = make_default_adapter elt in
  let adapter =
    match unbounded with
    | None -> adapter
    | Some x -> {adapter with is_unbounded = (fun () -> x)}
  in
  new t adapter ()

let destroy (r : #t) : unit = r#destroy ()

let layout (r : #t) : unit = r#layout ()
