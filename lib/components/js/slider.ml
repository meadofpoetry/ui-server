open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Slider
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

(* TODO
   - fully implement and test vertical slider *)

type event =
  | Mouse of Dom_html.mouseEvent Js.t
  | Touch of Dom_html.touchEvent Js.t

let page_factor = 10.

let quantize ~(step : float) (v : float) : float = Js.math##round (v /. step) *. step

let unwrap x = Js.Optdef.get x (fun () -> assert false)

let clamp ?(min = 0.) ?(max = 100.) v = Float.min (Float.max v min) max

let listen_body_lwt
    (typ : (#Dom_html.event as 'a) Js.t Dom_html.Event.typ)
    (handler : 'a Js.t -> unit Lwt.t -> unit Lwt.t) : unit Lwt.t =
  Js_of_ocaml_lwt.Lwt_js_events.(
    seq_loop (make_event typ) Dom_html.document##.body handler)

let get_touch_by_id (touches : Dom_html.touchList Js.t) (id : int) :
    Dom_html.touch Js.t option =
  let rec aux acc i =
    if i >= touches##.length
    then acc
    else
      let touch = unwrap (touches##item i) in
      if touch##.identifier = id then Some touch else aux acc (succ i)
  in
  aux None 0

let get_float_attribute (elt : Dom_html.element Js.t) (a : string) =
  match Element.get_attribute elt a with
  | None -> None
  | Some a -> float_of_string_opt a

module Attr = struct
  let aria_valuemin = "aria-valuemin"

  let aria_valuemax = "aria-valuemax"

  let aria_valuenow = "aria-valuenow"

  let aria_disabled = "aria-disabled"

  let data_step = "data-step"
end

module Selector = struct
  let track_marker_container = "." ^ CSS.track_marker_container

  let last_track_marker = "." ^ CSS.track_marker ^ ":last-child"

  let pin_value_marker = "." ^ CSS.pin_value_marker

  let thumb_container = "." ^ CSS.thumb_container

  let track_before = "." ^ CSS.track_before

  let track_after = "." ^ CSS.track_after
end

module Event = struct
  class type event = [float] Dom_html.customEvent

  let input : event Js.t Dom_html.Event.typ = Dom_html.Event.make "slider:input"

  let change : event Js.t Dom_html.Event.typ = Dom_html.Event.make "slider:change"
end

class t (elt : Dom_html.element Js.t) () =
  object (self)
    (* DOM nodes *)
    val track_marker_container =
      Element.query_selector elt Selector.track_marker_container

    val pin_value_marker = Element.query_selector elt Selector.pin_value_marker

    val thumb_container = Element.query_selector_exn elt Selector.thumb_container

    val track_before = Element.query_selector_exn elt Selector.track_before

    val track_after = Element.query_selector_exn elt Selector.track_after

    (* DOM touch event listeners *)
    val mutable _touchstart = None

    val mutable _touchmove = None

    val mutable _touchend = None

    (* DOM mouse event listeners *)
    val mutable _mouseenter = None

    val mutable _mouseleave = None

    val mutable _mousemove = None

    val mutable _mousedown = None

    val mutable _mouseup = None

    (* DOM event listeners *)
    val mutable _keydown = None

    val mutable _focus = None

    val mutable _blur = None

    val mutable _touch_id = None

    val mutable _prevent_focus_state = false

    val mutable _saved_tab_index = None

    val mutable _disabled = false

    val mutable _vertical = false

    val mutable _min = 0.

    val mutable _max = 100.

    val mutable _step = None

    val mutable _value = 0.

    inherit Widget.t elt () as super

    method! init () : unit =
      Js_of_ocaml_lwt.Lwt_js_events.(
        let blur = blurs super#root self#handle_blur in
        let focus = focuses super#root self#handle_focus in
        let keydown = keydowns super#root self#handle_keydown in
        (* Add interaction handlers *)
        let mousedown = mousedowns super#root self#handle_mouse_down in
        let touchstart = touchstarts super#root self#handle_touch_start in
        let touchmove = touchmoves super#root self#handle_touch_move in
        _blur <- Some blur;
        _focus <- Some focus;
        _keydown <- Some keydown;
        _mousedown <- Some mousedown;
        _touchstart <- Some touchstart;
        _touchmove <- Some touchmove);
      super#init ()

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      _vertical <- super#has_class CSS.vertical;
      let min' =
        match get_float_attribute super#root Attr.aria_valuemin with
        | None -> _min
        | Some x -> x
      in
      let max' =
        match get_float_attribute super#root Attr.aria_valuemax with
        | None -> _max
        | Some x -> x
      in
      if min' >= self#max
      then (
        self#set_max max';
        self#set_min min')
      else (
        self#set_min min';
        self#set_max max');
      _disabled <-
        (match super#get_attribute Attr.aria_disabled with
        | Some "true" -> true
        | _ -> false);
      (match get_float_attribute super#root Attr.data_step with
      | None -> ()
      | Some x -> _step <- Some x);
      (match get_float_attribute super#root Attr.aria_valuenow with
      | None -> ()
      | Some x -> _value <- x);
      (match self#step, self#discrete with
      | None, true | Some 0., true -> _step <- Some 1.
      | _ -> ());
      self#setup_track_marker ()

    method! destroy () : unit =
      super#destroy ();
      (* Stop DOM event listeners *)
      Option.iter Lwt.cancel _blur;
      _blur <- None;
      Option.iter Lwt.cancel _focus;
      _focus <- None;
      Option.iter Lwt.cancel _keydown;
      _keydown <- None;
      Option.iter Lwt.cancel _mousedown;
      _mousedown <- None;
      Option.iter Lwt.cancel _touchstart;
      _touchstart <- None;
      Option.iter Lwt.cancel _touchmove;
      _touchmove <- None

    method discrete : bool = super#has_class CSS.display_markers

    method has_track_marker : bool = super#has_class CSS.display_markers

    method disabled : bool = _disabled

    method set_disabled (x : bool) : unit =
      _disabled <- x;
      super#toggle_class ~force:x CSS.disabled;
      if x
      then (
        _saved_tab_index <- Element.get_attribute super#root "tabindex";
        super#set_attribute Attr.aria_disabled "true";
        super#remove_attribute "tabindex")
      else (
        super#remove_attribute Attr.aria_disabled;
        Option.iter (super#set_attribute "tabindex") _saved_tab_index)

    method vertical : bool = _vertical

    method min : float = _min

    method set_min (v : float) : unit =
      if v > self#max
      then raise (Invalid_argument "Min cannot be greater than max")
      else (
        _min <- v;
        self#set_value_ ~fire_input:false ~force:true self#value;
        super#set_attribute Attr.aria_valuemin (string_of_float v);
        self#setup_track_marker ())

    method max : float = _max

    method set_max (v : float) : unit =
      if v < self#min
      then raise (Invalid_argument "Max cannot be less than min")
      else (
        _max <- v;
        self#set_value_ ~fire_input:false ~force:true self#value;
        super#set_attribute Attr.aria_valuemax (string_of_float v);
        self#setup_track_marker ())

    method step : float option = _step

    method set_step (v : float option) : unit =
      match v with
      | None ->
          _step <- None;
          super#remove_attribute Attr.data_step;
          self#set_value_ ~fire_input:false ~force:true self#value;
          self#setup_track_marker ()
      | Some v when v < 0. -> raise (Invalid_argument "Step cannot be negative")
      | Some v ->
          let v = if self#discrete && v < 1. then 1. else v in
          _step <- Some v;
          super#set_attribute Attr.data_step (string_of_float v);
          self#set_value_ ~fire_input:false ~force:true self#value;
          self#setup_track_marker ()

    method value : float = _value

    method set_value (v : float) : unit = self#set_value_ ~fire_input:true v

    method step_up ?amount () =
      let amount =
        match amount with
        | Some x -> x
        | None -> (
          match self#step with
          | None -> 1.
          | Some x -> x)
      in
      self#set_value (self#value +. amount)

    method step_down ?amount () =
      let amount =
        match amount with
        | Some x -> x
        | None -> (
          match self#step with
          | None -> 1.
          | Some x -> x)
      in
      self#set_value (self#value +. amount)

    method private set_active_ (x : bool) : unit = super#toggle_class ~force:x CSS.active

    method private notify_input () : unit = super#emit ~detail:self#value Event.input

    method private notify_change () : unit = super#emit ~detail:self#value Event.change

    method private set_value_ ?(force = false) ~fire_input (v : float) : unit =
      let prev = self#value in
      let value = self#reduce_value v in
      match value with
      | None -> ()
      | Some v when Float.equal prev v && not force -> ()
      | Some v ->
          let min, max = self#min, self#max in
          let percent = clamp ((v -. min) *. 100. /. (max -. min)) in
          self#calculate_track_styles track_before percent;
          self#calculate_track_styles track_after (100. -. percent);
          self#calculate_thumb_styles percent;
          _value <- v;
          super#set_attribute Attr.aria_valuenow (string_of_float v);
          if fire_input
          then (
            self#notify_input ();
            if self#discrete then self#set_marker_value v)

    method private percent_to_value (percent : float) : float =
      let min, max = self#min, self#max in
      ((max -. min) *. percent /. 100.) +. min

    method private round_to_step ~(step : float) (value : float) : float =
      Float.round (value /. step) *. step

    method private get_offset (rect : Dom_html.clientRect Js.t) : float * float =
      let left, bottom = rect##.left, rect##.bottom in
      let window = Js.Unsafe.coerce Dom_html.window in
      let (page_y : float), (page_x : float) =
        window##.pageYOffset, window##.pageXOffset
      in
      bottom +. page_y, left +. page_x

    method private get_mouse_position : event -> float * float =
      function
      | Mouse e -> (
        match Js.Optdef.(to_option e##.pageX, to_option e##.pageY) with
        | Some page_x, Some page_y -> float_of_int page_x, float_of_int page_y
        | _ -> failwith "no page coordinates in mouse event")
      | Touch e -> (
          let touches = e##.changedTouches in
          let rec aux acc i =
            if i >= touches##.length
            then acc
            else
              let touch = unwrap (touches##item i) in
              match _touch_id with
              | None -> Some touch
              | Some id ->
                  if touch##.identifier = id then Some touch else aux acc (succ i)
          in
          match aux None 0 with
          | None -> failwith "no touch event found"
          | Some t -> float_of_int t##.pageX, float_of_int t##.pageY)

    method private calculate_thumb_styles (percent : float) : unit =
      let func = if self#vertical then "translateY" else "translateX" in
      let value =
        if self#vertical || super#is_rtl () then 100. -. percent else percent
      in
      let style = Printf.sprintf "%s(%g%%)" func value in
      thumb_container##.style##.transform := Js.string style

    method private calculate_track_styles
        (track : Dom_html.element Js.t)
        (percent : float)
        : unit =
      if self#disabled
      then
        let style = Printf.sprintf "calc(%g%% - 6px)" percent in
        if self#vertical
        then track##.style##.height := Js.string style
        else track##.style##.width := Js.string style
      else
        let v = percent /. 100. in
        let style =
          if self#vertical
          then
            Printf.sprintf
              "translateX(%s50%%) scaleY(%g)"
              (if super#is_rtl () then "" else "-")
              v
          else Printf.sprintf "translateY(-50%%) scaleX(%g)" v
        in
        track##.style##.transform := Js.string style

    method private remove_track_markers () : unit =
      Option.iter Element.remove_children track_marker_container

    method private append_track_markers (markers : int) : unit =
      Option.iter
        (fun container ->
          let frag = Dom_html.document##createDocumentFragment in
          let rec loop = function
            | 0 -> ()
            | i ->
                let marker = Dom_html.(createDiv document) in
                Element.add_class marker CSS.track_marker;
                Element.append_child frag marker;
                loop (pred i)
          in
          loop markers;
          Element.append_child container frag)
        track_marker_container

    method private set_marker_value (v : float) : unit =
      match pin_value_marker with
      | None -> ()
      | Some (elt : Element.t) ->
          elt##.textContent := Js.some (Js.string (string_of_float v))

    method private setup_track_marker () : unit =
      match self#step with
      | None -> ()
      | Some step ->
          if self#discrete && self#has_track_marker
          then (
            let markers' = (self#max -. self#min) /. step in
            let markers = ceil markers' in
            self#remove_track_markers ();
            self#append_track_markers (int_of_float markers);
            if markers' <> markers
            then
              let last_step_ratio =
                string_of_float @@ (((self#max -. (markers *. step)) /. step) +. 1.)
              in
              let flex =
                Animation.get_correct_property_name ~window:Dom_html.window "flex"
              in
              Element.query_selector super#root Selector.last_track_marker
              |> Option.iter (fun e -> Element.set_style_property e flex last_step_ratio))

    method private calculate_percent (e : event) =
      let rect = super#root##getBoundingClientRect in
      match Js.Optdef.to_option rect##.width, Js.Optdef.to_option rect##.height with
      | Some width, Some height ->
          let bottom, left = self#get_offset rect in
          let x, y = self#get_mouse_position e in
          let value = if self#vertical then bottom -. y else x -. left in
          let one_percent = if self#vertical then height /. 100. else width /. 100. in
          if super#is_rtl () && not self#vertical
          then 100. -. clamp (value /. one_percent)
          else clamp (value /. one_percent)
      | _ -> 0.

    method private reduce_value (raw_value : float) : float option =
      if self#disabled
      then None
      else
        match self#step with
        | None -> Some raw_value
        | Some step -> Some (quantize ~step raw_value)

    method private handle_focus _ _ : unit Lwt.t =
      if not _prevent_focus_state then super#add_class CSS.focus;
      Lwt.return_unit

    method private handle_blur _ _ : unit Lwt.t =
      _prevent_focus_state <- false;
      super#remove_class CSS.focus;
      Lwt.return_unit

    method private handle_mouse_enter (e : Dom_html.mouseEvent Js.t) _ : unit Lwt.t =
      if (Js.Unsafe.coerce e)##.buttons = 0 then self#handle_drag_end ();
      Lwt.return_unit

    method private handle_mouse_leave (e : Dom_html.mouseEvent Js.t) _ : unit Lwt.t =
      self#handle_mouse_move e Lwt.return_unit

    method private handle_touch_start (e : Dom_html.touchEvent Js.t) _ : unit Lwt.t =
      Dom.preventDefault e;
      (match Js.Optdef.to_option (e##.changedTouches##item 0) with
      | None -> ()
      | Some touch -> _touch_id <- Some touch##.identifier);
      _prevent_focus_state <- true;
      self#set_active_ true;
      _touchend <- Some (listen_body_lwt Dom_html.Event.touchend self#handle_touch_end);
      Lwt.return_unit

    method private handle_mouse_down
        (e : Dom_html.mouseEvent Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      Dom.preventDefault e;
      _prevent_focus_state <- true;
      (* FIXME workaround, why we don't gain focus on click natively?? *)
      super#root##focus;
      self#set_active_ true;
      _mouseenter <-
        Some (listen_body_lwt (Dom_html.Event.make "mouseenter") self#handle_mouse_enter);
      _mouseleave <-
        Some (listen_body_lwt (Dom_html.Event.make "mouseleave") self#handle_mouse_leave);
      _mousemove <-
        Some (listen_body_lwt Dom_html.Event.mousemove self#handle_mouse_move);
      _mouseup <- Some (listen_body_lwt Dom_html.Event.mouseup self#handle_mouse_up);
      self#handle_move (Mouse e)

    method private handle_touch_end
        (e : Dom_html.touchEvent Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      (match _touch_id with
      | None -> self#handle_drag_end ()
      | Some id -> (
          let touches = e##.changedTouches in
          match get_touch_by_id touches id with
          | None -> ()
          | Some _ -> self#handle_drag_end ()));
      Lwt.return_unit

    method private handle_mouse_up
        (_ : Dom_html.mouseEvent Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      self#handle_drag_end ();
      Lwt.return_unit

    method private handle_touch_move
        (e : Dom_html.touchEvent Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      match _touch_id with
      | None -> self#handle_move (Touch e)
      | Some id -> (
          let touches = e##.changedTouches in
          match get_touch_by_id touches id with
          | None -> Lwt.return_unit
          | Some _ -> self#handle_move (Touch e))

    method private handle_mouse_move
        (e : Dom_html.mouseEvent Js.t)
        (_ : unit Lwt.t)
        : unit Lwt.t =
      self#handle_move ~fire_input:true (Mouse e)

    method private handle_move ?(fire_input = true) (e : event) : unit Lwt.t =
      let percent = self#calculate_percent e in
      let value = self#percent_to_value percent in
      self#set_value_ ~fire_input value;
      Lwt.return_unit

    method private handle_drag_end () : unit =
      self#set_active_ false;
      self#notify_change ();
      Option.iter Lwt.cancel _mouseenter;
      _mouseenter <- None;
      Option.iter Lwt.cancel _mouseleave;
      _mouseleave <- None;
      Option.iter Lwt.cancel _mousemove;
      _mousemove <- None;
      Option.iter Lwt.cancel _mouseup;
      _mouseup <- None;
      Option.iter Lwt.cancel _touchend;
      _touchend <- None

    method private handle_keydown (e : Dom_html.keyboardEvent Js.t) _ : unit Lwt.t =
      let key = Dom_html.Keyboard_code.of_event e in
      let min, max, value = self#min, self#max, self#value in
      let one_percent = Float.abs ((max -. min) /. 100.) in
      let step =
        match self#step with
        | None -> one_percent
        | Some x -> x
      in
      let value =
        match key with
        | ArrowLeft | ArrowDown -> Some (value -. step)
        | ArrowRight | ArrowUp -> Some (value +. step)
        | Home -> Some min
        | End -> Some max
        | PageUp -> Some (value +. (one_percent *. page_factor))
        | PageDown -> Some (value -. (one_percent *. page_factor))
        | _ -> None
      in
      match value with
      | None -> Lwt.return_unit
      | Some value ->
          Dom.preventDefault e;
          let value = clamp ~min ~max value in
          super#add_class CSS.focus;
          self#set_value_ ~fire_input:true value;
          self#notify_change ();
          Lwt.return_unit
  end

(** Attach slider widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

(** Create new slider from scratch *)
let make
    ?classes
    ?a
    ?discrete
    ?markers
    ?disabled
    ?label
    ?step
    ?track_before
    ?track_after
    ?thumb_container
    ?track_marker_container
    ?container
    ?min
    ?max
    ?value
    () : t =
  D.slider
    ?classes
    ?a
    ?discrete
    ?markers
    ?disabled
    ?label
    ?step
    ?track_before
    ?track_after
    ?thumb_container
    ?track_marker_container
    ?container
    ?min
    ?max
    ?value
    ()
  |> Tyxml_js.To_dom.of_element
  |> attach
