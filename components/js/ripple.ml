open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_tyxml.Ripple.Make(Xml)(Svg)(Html)

type frame =
  { width : float
  ; height : float
  }

type coords =
  { left : float
  ; top : float
  }

type activation_state =
  { is_activated : bool
  ; has_deactivation_ux_run : bool
  ; was_activated_by_pointer : bool
  ; was_element_made_active : bool
  ; activation_event : Dom_html.event Js.t option
  ; is_programmatic : bool
  }

type point =
  { x : int
  ; y : int
  }

let default_activation_state =
  { is_activated = false
  ; has_deactivation_ux_run = false
  ; was_activated_by_pointer = false
  ; was_element_made_active = false
  ; activation_event = None
  ; is_programmatic = false
  }

module Util = struct

  let supports_passive : bool option ref = ref None

  let suppots_css_variables_ : bool option ref = ref None

  (* FIXME implement *)
  let apply_passive ?(global_obj = Dom_html.window)
        ?(force_refresh = false) () =
    ignore global_obj;
    match !supports_passive, force_refresh with
    | None, _ | _, true ->
       (try ()
        with _ -> ())
    | _ -> ()

  let detect_edge_pseudo_var_bug (window : Dom_html.window Js.t) : bool =
    let doc = window##.document in
    let node = doc##createElement (Js.string "div") in
    node##.className := Js.string "mdc-ripple-surface--test-edge-var-bug";
    ignore (doc##.body##appendChild (node :> Dom.node Js.t));
    let (computed_style : Dom_html.cssStyleDeclaration Js.t Js.opt) =
      (Js.Unsafe.coerce window)##getComputedStyle node in
    let has_pseudo_var_bug = match Js.Opt.to_option computed_style with
      | None -> true
      | Some s -> String.equal "solid" (Js.to_string s##.borderTopStyle) in
    ignore @@ doc##.body##removeChild (node :> Dom.node Js.t);
    has_pseudo_var_bug

  let supports_css_variables ?(force_refresh = false)
        (window : Dom_html.window Js.t) : bool =
    match force_refresh, !suppots_css_variables_ with
    | false, Some x -> x
    | true, _ | _, None ->
       let window = Js.Unsafe.coerce window in
       let supports_function_present =
         if not @@ Js.Optdef.test window##.CSS then false else
           Js.Optdef.test window##.CSS##.supports
           && String.equal
                "function"
                (Js.to_string (Js.typeof window##.CSS##.supports)) in
       if not supports_function_present
       then false
       else begin
           let explicitly_supports_css_vars =
             window##.CSS##supports
               (Js.string "--css-vars")
               (Js.string "yes")
             |> Js.to_bool in
           let we_are_feature_detecting_safary_10_plus =
             let a = Js.to_bool
                     @@ window##.CSS##supports
                          (Js.string "(--css-vars: yes)") in
             let b = Js.to_bool
                     @@ window##.CSS##supports
                          (Js.string "color")
                          (Js.string "#00000000") in
             a && b in
           let supports =
             if explicitly_supports_css_vars
                || we_are_feature_detecting_safary_10_plus
             then not @@ detect_edge_pseudo_var_bug window
             else false in
           if force_refresh then suppots_css_variables_ := Some supports;
           supports
         end

  let get_normalized_event_coords (event : Dom_html.event Js.t)
        (page_offset : point)
        (client_rect : Widget.rect) =
    let { x; y } = page_offset in
    let doc_x = x + int_of_float client_rect.left in
    let doc_y = y + int_of_float client_rect.top in
    let normalized_x, normalized_y = match Js.to_string event##._type with
    | "touchstart" ->
       let (ev : Dom_html.touchEvent Js.t) =
         Js.Unsafe.coerce event in
       let (touch : Dom_html.touch Js.t) =
         Js.Optdef.get (ev##.changedTouches##item 0) (fun () -> assert false) in
       touch##.pageX - doc_x,
       touch##.pageY - doc_y
    | _ ->
       let (ev : Dom_html.mouseEvent Js.t) =
         Js.Unsafe.coerce event in
       let page_x = Js.Optdef.get ev##.pageX (fun () -> 0) in
       let page_y = Js.Optdef.get ev##.pageY (fun () -> 0) in
       page_x - doc_x,
       page_y - doc_y in
    { x = normalized_x
    ; y = normalized_y
    }

  (* FIXME add method availability check *)
  let get_matches_property (elt : Dom_html.element Js.t) s =
    (Js.Unsafe.coerce elt)##matches (Js.string s)

end

type adapter =
  { add_class : string -> unit
  ; remove_class : string -> unit
  ; is_unbounded : unit -> bool
  ; is_surface_active : unit -> bool
  ; is_surface_disabled : unit -> bool
  ; register_handler : string -> (Dom_html.event Js.t -> unit) -> handler
  ; deregister_handler : handler -> unit
  ; contains_event_target : Dom_html.element Js.t -> bool
  ; update_css_variable : string -> string option -> unit
  ; compute_bounding_rect : unit -> Widget.rect
  }
and handler = Dom_events.listener

exception Return

let update_css_variable = fun node name value ->
  (Js.Unsafe.coerce node##.style)##setProperty
    (Js.string name)
    (Js.string @@ Option.get_or ~default:"" value)

let make_default_adapter (w : #Widget.t) : adapter =
  { add_class = w#add_class
  ; remove_class = w#remove_class
  ; is_unbounded = (fun () -> false)
  ; is_surface_active = (fun () ->
    Util.get_matches_property w#root ":active"
    |> Js.to_bool)
  ; is_surface_disabled = (fun () ->
    if Js.Optdef.test (Js.Unsafe.coerce w#root)##.disabled
    then Js.to_bool (Js.Unsafe.coerce w#root)##.disabled else false)
  ; register_handler = (fun typ f ->
    w#listen (Widget.Event.make typ) (fun _ e ->
        f (e :> Dom_html.event Js.t); true))
  ; deregister_handler = (fun x ->
    Dom_events.stop_listen x)
  ; contains_event_target = (fun target ->
    (Js.Unsafe.coerce w#root)##contains target |> Js.to_bool)
  ; update_css_variable = (fun name value ->
    update_css_variable w#root name value)
  ; compute_bounding_rect = (fun () -> w#bounding_client_rect)
  }

let padding = 20

let initial_origin_scale = 0.6

let deactivation_timeout_ms = 225.
let fg_deactivation_ms = 150.
let tap_delay_ms = 300.

let activation_event_types =
  ["touchstart"; "pointerdown"; "mousedown"; "keydown"]

let pointer_deactivation_event_types =
  ["touchend"; "pointerup"; "mouseup"]

class t (adapter : adapter) () =
object(self)

  val mutable _layout_frame = None
  val mutable _frame = { width = 0.; height = 0. }
  val mutable _activation_state = default_activation_state
  val mutable _initial_size = 0.
  val mutable _max_radius = 0.
  val mutable _fg_scale = 0.
  val mutable _unbounded_coords = { left = 0.; top = 0. }
  val mutable _activation_timer = None
  val mutable _fg_deactivation_removal_timer = None
  val mutable _activation_animation_has_ended = false
  val mutable _previous_activation_event : Dom_html.event Js.t option = None
  val mutable _activated_targets = []

  val mutable _root_listeners = []
  val mutable _deactivation_listeners = []

  method unbounded : bool =
    adapter.is_unbounded ()

  method set_unbounded (x : bool) : unit =
    if x then adapter.add_class Markup.unbounded_class
    else adapter.remove_class Markup.unbounded_class

  method activate ?(event : Dom_html.event Js.t option) () : unit =
    self#activate_ event

  method deactivate () : unit =
    self#deactivate_ ()

  method layout () : unit =
    Option.iter Utils.Animation.cancel_animation_frame _layout_frame;
    let f = fun _ ->
      self#layout_internal ();
      _layout_frame <- None in
    _layout_frame <- Some (Utils.Animation.request_animation_frame f)

  (* Private methods *)

  method init () : unit =
    let supports_press_ripple = self#supports_press_ripple () in
    self#register_root_handlers(supports_press_ripple);
    if supports_press_ripple
    then
      begin
        let f = fun _ ->
          adapter.add_class Markup.base_class;
          if adapter.is_unbounded ()
          then (adapter.add_class Markup.unbounded_class;
                self#layout_internal ()) in
        ignore @@ Utils.Animation.request_animation_frame f
      end

  method destroy () =
    if self#supports_press_ripple ()
    then
      (begin match _activation_timer with
       | None -> ()
       | Some x ->
          Dom_html.clearTimeout x;
          _activation_timer <- None;
          adapter.remove_class Markup.fg_activation_class
       end;
       begin match _fg_deactivation_removal_timer with
       | None -> ()
       | Some x ->
          Dom_html.clearTimeout x;
          _fg_deactivation_removal_timer <- None;
          adapter.remove_class Markup.fg_deactivation_class
       end;
       let f = fun _ ->
         adapter.remove_class Markup.base_class;
         adapter.remove_class Markup.unbounded_class;
         self#remove_css_vars () in
       ignore @@ Utils.Animation.request_animation_frame f)
    else
      (self#deregister_root_handlers ();
       self#deregister_deactivation_handlers ())

  (* We compute this property so that we are not querying information about
   * the client until the point in time where the foundation requests it.
   * This prevents scenarios where client-side feature-detection may happen
   * too early, such as when components are rendered on the server
   * and then initialized at mount time on the client. *)
  method private supports_press_ripple () : bool =
    Util.supports_css_variables Dom_html.window

  method private register_root_handlers (supports_press_ripple : bool) : unit =
    let listeners =
      if not @@ supports_press_ripple then [] else
        let rsz =
          Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ ->
              self#layout (); true) in
        let handler = fun event ->
          self#activate ~event () in
        let oth = List.map (fun x -> adapter.register_handler x handler)
                    activation_event_types in
        rsz :: oth in
    let listeners =
      adapter.register_handler "focus" (fun _ -> self#handle_focus ())
      :: adapter.register_handler "blur" (fun _ -> self#handle_blur ())
      :: listeners in
    _root_listeners <- _root_listeners @ listeners

  method register_deactivation_handlers (event : Dom_html.event Js.t) =
    let handler = fun _ -> self#deactivate () in
    match Js.to_string event##._type with
    | "keydown" ->
       let listener = adapter.register_handler "keyup" handler in
       _deactivation_listeners <- listener :: _deactivation_listeners;
    | _ ->
       pointer_deactivation_event_types
       |> List.map (fun x -> adapter.register_handler x handler)
       |> fun l -> _deactivation_listeners <- _deactivation_listeners @ l

  method private deregister_root_handlers () : unit =
    List.iter adapter.deregister_handler _root_listeners;
    _root_listeners <- []

  method private deregister_deactivation_handlers () : unit =
    List.iter adapter.deregister_handler _deactivation_listeners;
    _deactivation_listeners <- []

  method private activation_timer_callback () : unit =
    _activation_animation_has_ended <- true;
    self#run_deactivation_ux_logic_if_ready ()

  method private remove_css_vars () : unit =
    List.iter (fun v -> adapter.update_css_variable v None) Markup.vars

  method activate_ (event : Dom_html.event Js.t option) : unit =
    let is_same_interaction () =
      match _previous_activation_event, event with
      | None, _ | _, None -> false
      | Some event, Some e ->
         let typ = Js.to_string event##._type in
         let prev_typ = Js.to_string e##._type in
         not @@ String.equal typ prev_typ in
    let has_activated_child () =
      match event with
      | None -> false
      | Some _ ->
         List.find_opt adapter.contains_event_target _activated_targets
         |> Option.is_some in
    try
      if adapter.is_surface_disabled () then raise_notrace Return;
      if _activation_state.is_activated then raise_notrace Return;
      if is_same_interaction () then raise_notrace Return;
      if has_activated_child ()
      then (self#reset_activation_state ();
            raise_notrace Return);

      Option.iter (fun e ->
          Js.Opt.iter e##.target (fun x ->
              _activated_targets <- x :: _activated_targets);
          self#register_deactivation_handlers e) event;

      let is_activated = true in
      let is_programmatic = Option.is_none event in
      let activation_event = event in
      let was_activated_by_pointer = match event with
        | None -> false
        | Some e ->
           let typ = Js.to_string e##._type in
           let lst = ["mousedown"; "touchstart"; "pointerdown"] in
           List.mem ~eq:String.equal typ lst in
      let was_element_made_active = self#check_element_made_active event in
      let state = { _activation_state with is_activated
                                         ; is_programmatic
                                         ; activation_event
                                         ; was_element_made_active
                                         ; was_activated_by_pointer } in
      _activation_state <- state;
      if was_element_made_active then self#animate_activation ();
      Utils.Animation.request_animation_frame (fun _ ->
          _activated_targets <- [];
          if (not was_element_made_active)
             && Option.map_or ~default:false (fun e ->
                    let e = Js.Unsafe.coerce e in
                    let key = Option.map Js.to_string
                              @@ Js.Optdef.to_option e##.key in
                    let key_code = Js.Optdef.to_option e##.keyCode in
                    begin match key, key_code with
                    | Some " ", _ | _, Some 32 -> true
                    | _ -> false
                    end) event
          then begin
              let was_element_made_active =
                self#check_element_made_active event in
              if was_element_made_active then self#animate_activation ();
              let state = { _activation_state with was_element_made_active } in
              _activation_state <- state;
            end;
          (* Reset activation state immediately
           * if element was not made active. *)
          if not _activation_state.was_element_made_active
          then _activation_state <- default_activation_state)
      |> ignore;
    with
    | Return -> ()

  method check_element_made_active (event : Dom_html.event Js.t option) : bool =
    let eq s e = String.equal s (Js.to_string e##._type) in
    match event with
    | Some e when eq "keydown" e -> adapter.is_surface_active ()
    | _ -> true

  method private animate_activation () : unit =
    self#layout_internal ();
    let translate_start, translate_end =
      if self#unbounded then "", "" else
        let start_point, end_point = self#get_fg_translation_coordinates () in
        Printf.sprintf "%dpx, %dpx" start_point.x start_point.y,
        Printf.sprintf "%dpx, %dpx" end_point.x end_point.y in
    adapter.update_css_variable
      Markup.var_fg_translate_start
      (Some translate_start);
    adapter.update_css_variable
      Markup.var_fg_translate_end
      (Some translate_end);
    (* Cancel any ongoing activation/deactivation animations *)
    Option.iter Dom_html.clearTimeout _activation_timer;
    _activation_timer <- None;
    Option.iter Dom_html.clearTimeout _fg_deactivation_removal_timer;
    _fg_deactivation_removal_timer <- None;
    self#rm_bounded_activation_classes ();
    adapter.remove_class Markup.fg_deactivation_class;
    (* Force layout in order to re-trigger the animation. *)
    ignore @@ adapter.compute_bounding_rect ();
    adapter.add_class Markup.fg_activation_class;
    let cb = self#activation_timer_callback in
    let timeout = deactivation_timeout_ms in
    _activation_timer <- Some (Utils.set_timeout cb timeout)

  method private get_fg_translation_coordinates () : point * point =
    let start_point = match _activation_state.was_activated_by_pointer with
      | true ->
         let window_page_offset =
           { x = (Js.Unsafe.coerce Dom_html.window)##.pageXOffset
           ; y = (Js.Unsafe.coerce Dom_html.window)##.pageYOffset
           } in
         Util.get_normalized_event_coords
           (Option.get_exn _activation_state.activation_event)
           window_page_offset
           (adapter.compute_bounding_rect ())
      | false ->
         { x = int_of_float @@ _frame.width /. 2.
         ; y = int_of_float @@ _frame.height /. 2.} in
    let start_point =
      { x = start_point.x - (int_of_float (_initial_size /. 2.))
      ; y = start_point.y - (int_of_float (_initial_size /. 2.))
      } in
    let end_point =
      { x = int_of_float @@ (_frame.width /. 2.) -. (_initial_size /. 2.)
      ; y = int_of_float @@ (_frame.height /. 2.) -. (_initial_size /. 2.)
      } in
    start_point, end_point

  method private run_deactivation_ux_logic_if_ready () : unit =
    (* This method is called both when a pointing device is released,
     * and when the activation animation ends.
     * The deactivation animation should only run after both of those occur.
     *)
    let { has_deactivation_ux_run; is_activated; _ } = _activation_state in
    let activation_has_ended = has_deactivation_ux_run || (not is_activated) in
    if activation_has_ended && _activation_animation_has_ended
    then
      (self#rm_bounded_activation_classes ();
       adapter.add_class Markup.fg_deactivation_class;
       Utils.set_timeout (fun () ->
           adapter.remove_class Markup.fg_deactivation_class)
         fg_deactivation_ms
       |> fun timer_id -> _fg_deactivation_removal_timer <- Some timer_id)

  method private rm_bounded_activation_classes () : unit =
    adapter.remove_class Markup.fg_activation_class;
    _activation_animation_has_ended <- false;
    ignore @@ adapter.compute_bounding_rect ()

  method private reset_activation_state () : unit =
    _previous_activation_event <- _activation_state.activation_event;
    _activation_state <- default_activation_state;
    (* Touch devices may fire additional events for the
     * same interaction within a short time. Store the previous
     * event until it's safe to assume that subsequent events are
     *  for new interactions.
     *)
    let cb = fun () -> _previous_activation_event <- None in
    ignore @@ Utils.set_timeout cb tap_delay_ms

  method private deactivate_ () : unit =
    let ({ is_activated; is_programmatic; _ } as state) = _activation_state in
    match is_activated, is_programmatic with
    | false, _ -> ()
    | _, true ->
       let cb = fun _ -> self#animate_deactivation state in
       ignore @@ Utils.Animation.request_animation_frame cb;
       self#reset_activation_state ()
    | _, false ->
       self#deregister_deactivation_handlers ();
       let f = fun _ ->
         let state = { state with has_deactivation_ux_run = true } in
         _activation_state <- state;
         self#animate_deactivation state;
         self#reset_activation_state () in
       ignore @@ Utils.Animation.request_animation_frame f

  method private animate_deactivation (a : activation_state) =
    if a.was_activated_by_pointer || a.was_element_made_active
    then self#run_deactivation_ux_logic_if_ready ()

  method private layout_internal () : unit =
    let rect = adapter.compute_bounding_rect () in
    let width = Option.get_exn rect.width in
    let height = Option.get_exn rect.height in
    _frame <- { width; height };
    let max_dim = Float.max width height in
    let get_bounded_radius () =
      let hypotenuse = sqrt ((width *. width) +. (height *. height)) in
      hypotenuse +. float_of_int padding in
    let max_radius =
      if self#unbounded then max_dim else get_bounded_radius () in
    let initial_size = floor @@ max_dim *. initial_origin_scale in
    let fg_scale = max_radius /. initial_size in
    _max_radius <- max_radius;
    _initial_size <- initial_size;
    _fg_scale <- fg_scale;
    self#update_layout_css_vars ()

  method private update_layout_css_vars () : unit =
    adapter.update_css_variable Markup.var_fg_size
      (Some (Printf.sprintf "%dpx" @@ int_of_float _initial_size));
    adapter.update_css_variable Markup.var_fg_scale
      (Some (Printf.sprintf "%g" _fg_scale));
    if self#unbounded
    then
      (let left = Float.(round ((_frame.width / 2.) - (_initial_size / 2.))) in
       let top = Float.(round ((_frame.height / 2.) - (_initial_size / 2.))) in
       _unbounded_coords <- { left; top };
       adapter.update_css_variable Markup.var_left
         (Some (Printf.sprintf "%gpx" left));
       adapter.update_css_variable Markup.var_top
         (Some (Printf.sprintf "%gpx" top)))

  method private handle_focus () : unit =
    let cb = fun _ -> adapter.add_class Markup.bg_focused_class in
    ignore @@ Utils.Animation.request_animation_frame cb

  method private handle_blur () : unit =
    let cb = fun _ -> adapter.remove_class Markup.bg_focused_class in
    ignore @@ Utils.Animation.request_animation_frame cb

  initializer
    self#set_unbounded @@ adapter.is_unbounded ();
    self#init ()

end

let attach_to ?unbounded (w : #Widget.t) : t =
  let adapter = make_default_adapter w in
  let adapter = match unbounded with
    | None -> adapter
    | Some x -> { adapter with is_unbounded = (fun () -> x) } in
  new t adapter ()

let destroy (r : #t) : unit = r#destroy ()

let layout (r : #t) : unit = r#layout ()
