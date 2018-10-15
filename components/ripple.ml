open Containers
open Tyxml_js

module Markup = Components_markup.Ripple.Make(Xml)(Svg)(Html)

type adapter =
  { add_class : string -> unit
  ; remove_class : string -> unit
  ; is_unbounded : unit -> bool
  ; is_surface_active : unit -> bool
  ; is_surface_disabled : unit -> bool
  ; register_handler : string -> (Dom_html.event Js.t -> unit) -> handler
  ; deregister_handler : handler -> unit
  ; contains_event_target : Dom_html.element Js.t -> bool
  ; update_css_variable : string -> string -> unit
  ; compute_bounding_rect : unit -> Widget.rect
  }
and handler = Dom_events.listener

let default_adapter (w : #Widget.t) : adapter =
  { add_class = w#add_class
  ; remove_class = w#remove_class
  ; is_unbounded = (fun () -> false)
  ; is_surface_active = (fun () -> false)
  ; is_surface_disabled = (fun () -> false)
  ; register_handler = (fun typ f ->
    w#listen (Widget.Event.make typ) (fun _ e ->
        f (e :> Dom_html.event Js.t); true))
  ; deregister_handler = (fun x ->
    Dom_events.stop_listen x)
  ; contains_event_target = (fun _ -> true)
  ; update_css_variable = (fun name value ->
    (Js.Unsafe.coerce w#style)##setProperty
      (Js.string name)
      (Js.string value))
  ; compute_bounding_rect = (fun () -> w#bounding_client_rect)
  }

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

  let apply_passive ?(global_obj = Dom_html.window)
        ?(force_refresh = false) () =
    match !supports_passive, force_refresh with
    | None, _ | _, true ->
       (try ()
        with _ -> ())
    | _ -> ()

  let supports_css_variables ?(force_refresh = false)
        (window : Dom_html.window Js.t) =
    (* TODO *)
    ignore force_refresh;
    ignore window;
    true

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



end

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
  val mutable _previous_activation_event = None

  val mutable _root_listeners = []
  val mutable _deactivation_listeners = []

  method unbounded : bool =
    adapter.is_unbounded ()

  method set_unbounded (x : bool) : unit =
    if x then adapter.add_class Markup.unbounded_class
    else adapter.remove_class Markup.unbounded_class

  method activate (e : Dom_html.event Js.t) : unit =
    let typ = Js.to_string e##._type in
    let is_programmatic = _activation_state.is_programmatic in
    let _ =
      if is_programmatic
      then false
      else List.mem ~eq:String.equal typ
             ["mousedown"; "touchstart"; "pointerdown"] in
    ()

  method deactivate () : unit =
    if _activation_state.is_activated
    then
      if _activation_state.is_programmatic
      then
        (let f = fun _ -> self#animate_deactivation _activation_state in
         let cb = Js.wrap_callback f in
         ignore @@ Dom_html.window##requestAnimationFrame cb;
         self#reset_activation_state ())
      else
        (self#deregister_deactivation_handlers ();
         let _ = fun _ ->
           let state =
             { _activation_state with has_deactivation_ux_run = true } in
           ignore state;
           self#animate_deactivation _activation_state;
           self#reset_activation_state () in
         ())

  method layout () : unit =
    let window = Dom_html.window in
    Option.iter (fun x -> window##cancelAnimationFrame x)_layout_frame;
    let cb = Js.wrap_callback (fun _ ->
                 self#layout_internal ();
                 _layout_frame <- None) in
    _layout_frame <- Some (window##requestAnimationFrame cb)

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
        let cb = Js.wrap_callback f in
        ignore @@ Dom_html.window##requestAnimationFrame cb
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
       let cb = Js.wrap_callback f in
       ignore @@ Dom_html.window##requestAnimationFrame cb)
    else
      (self#deregister_root_handlers ();
       self#deregister_deactivation_handlers ())

  method private remove_css_vars () : unit =
    (* TODO *)
    ()

  (* We compute this property so that we are not querying information about
   * the client until the point in time where the foundation requests it.
   * This prevents scenarios where client-side feature-detection may happen
   * too early, such as when components are rendered on the server
   * and then initialized at mount time on the client. *)
  method private supports_press_ripple () : bool =
    (* TODO *)
    true

  method private register_root_handlers (supports_press_ripple : bool) : unit =
    let listeners =
      if not @@ supports_press_ripple then [] else
        let rsz =
          Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ ->
            self#layout (); true) in
        let oth = List.map (fun x -> adapter.register_handler x self#activate)
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

  method private animate_activation () : unit =
    self#layout_internal ();
    let translate_start, translate_end =
      if not @@ self#unbounded then "", "" else
        let start_point, end_point = self#get_fg_translation_coordinates () in
        Printf.sprintf "%dpx, %dpx" start_point.x start_point.y,
        Printf.sprintf "%dpx, %dpx" end_point.x end_point.y in
    adapter.update_css_variable Markup.var_fg_translate_start translate_start;
    adapter.update_css_variable Markup.var_fg_translate_end translate_end;
    (* Cancel any ongoing activation/deactivation animations *)
    Option.iter Dom_html.clearTimeout _activation_timer;
    Option.iter Dom_html.clearTimeout _fg_deactivation_removal_timer;
    adapter.remove_class Markup.fg_deactivation_class;
    (* Force layout in order to re-trigger the animation. *)
    ignore @@ adapter.compute_bounding_rect ();
    adapter.add_class Markup.fg_activation_class;
    let cb = self#activation_timer_callback in
    let timeout = deactivation_timeout_ms in
    _activation_timer <- Some (Dom_html.setTimeout cb timeout)

  method private get_fg_translation_coordinates () : point * point =
    let _ = _activation_state.activation_event in
    let _ = _activation_state.was_activated_by_pointer in
    let end_point =
      { x = int_of_float @@ (_frame.width /. 2.) -. (_initial_size /. 2.)
      ; y = int_of_float @@ (_frame.height /. 2.) -. (_initial_size /. 2.)
      } in
    { x = 0; y = 0 }, end_point

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
       Dom_html.setTimeout (fun () ->
           adapter.remove_class Markup.fg_deactivation_class)
         fg_deactivation_ms
       |> fun timer_id -> _fg_deactivation_removal_timer <- Some timer_id)

  method private rm_bounded_activation_classes () : unit =
    adapter.remove_class Markup.fg_activation_class;
    _activation_animation_has_ended <- false;
    ignore @@ adapter.compute_bounding_rect ()

  method private reset_activation_state () : unit =
    (* TODO *)
    ()

  method private animate_deactivation _ =
    ()

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
      (Printf.sprintf "%dpx" @@ int_of_float _initial_size);
    adapter.update_css_variable Markup.var_fg_scale
      (Printf.sprintf "%g" _fg_scale);
    if self#unbounded
    then
      (let left = Float.(round ((_frame.width / 2.) - (_initial_size / 2.))) in
       let top = Float.(round ((_frame.height / 2.) - (_initial_size / 2.))) in
       _unbounded_coords <- { left; top };
       adapter.update_css_variable Markup.var_left
         (Printf.sprintf "%gpx" left);
       adapter.update_css_variable Markup.var_top
         (Printf.sprintf "%gpx" top))

  method private handle_focus () : unit =
    (fun _ -> adapter.add_class Markup.bg_focused_class)
    |> Js.wrap_callback
    |> (fun cb -> Dom_html.window##requestAnimationFrame cb)
    |> ignore

  method private handle_blur () : unit =
    (fun _ -> adapter.remove_class Markup.bg_focused_class)
    |> Js.wrap_callback
    |> (fun cb -> Dom_html.window##requestAnimationFrame cb)
    |> ignore

  initializer
    self#init ()

end

(* TODO remove *)
class type mdc =
  object
    method activate   : unit -> unit Js.meth
    method deactivate : unit -> unit Js.meth
    method layout     : unit -> unit Js.meth
    method unbounded  : bool Js.t Js.prop
  end

let attach (elt : #Widget.t) : mdc Js.t =
  (* elt#add_class Markup.Ripple.base_class; *)
  Js.Unsafe.global##.mdc##.ripple##.MDCRipple##attachTo elt#root

let attach' (elt : Dom_html.element Js.t) : mdc Js.t =
  Js.Unsafe.global##.mdc##.ripple##.MDCRipple##attachTo elt

let set_unbounded (elt:#Widget.t)    = elt#set_attribute "data-mdc-ripple-is-unbounded" ""
let remove_unbounded (elt:#Widget.t) = elt#remove_attribute "data-mdc-ripple-is-unbounded"
