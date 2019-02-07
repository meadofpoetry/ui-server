open Js_of_ocaml
open Js_of_ocaml_lwt
open Containers
open Tyxml_js

type event =
  | Mouse of Dom_html.mouseEvent Js.t
  | Touch of Dom_html.touchEvent Js.t

let coerce_event = function
  | Mouse e -> (e :> Dom_html.event Js.t)
  | Touch e -> (e :> Dom_html.event Js.t)

let wrap_mouse e = Mouse e
let wrap_touch e = Touch e

let page_factor = 4.

let quantize ~(step : float) (v : float) : float =
  let steps = int_of_float @@ Js.math##round (v /. step) in
  (float_of_int steps) *. step

let move_event_map =
  [ "mousedown", "mousemove"
  ; "touchstart", "touchmove"
  ; "pointerdown", "pointermove"
  ]

module Attr = struct

  let min = "aria-valuemin"
  let max = "aria-valuemax"
  let now = "aria-valuenow"
  let disabled = "aria-disabled"
  let step = "data-step"

end

module Markup = Components_tyxml.Slider.Make(Xml)(Svg)(Html)

let string_of_float (f : float) : string =
  Markup.string_of_float f

class t (elt : #Dom_html.element Js.t) () =
  let e_input, set_input = React.E.create () in
  let e_change, set_change = React.E.create () in
  object(self)

    inherit Widget.t elt () as super

    (* Elements *)
    val thumb_container =
      let selector = "." ^ Markup.CSS.thumb_container in
      Element.query_selector elt selector
    val track =
      let selector = "." ^ Markup.CSS.track in
      Element.query_selector elt selector
    val pin_value_marker =
      let selector = "." ^ Markup.CSS.pin_value_marker in
      Element.query_selector elt selector
    val track_marker_container =
      let selector = "." ^ Markup.CSS.track_marker_container in
      Element.query_selector elt selector

    (* Event handlers *)
    val mutable down_handlers = []
    val mutable up_handlers = []
    val mutable move_handler = None
    val mutable keydown_handler = None
    val mutable focus_handler = None
    val mutable blur_handler = None
    val mutable transitionend_handler = None
    val mutable resize_handler = None

    val mutable s_input = None

    val mutable saved_tab_index = None
    val mutable active = false
    val mutable in_transit = false
    val mutable handling_thumb_target_evt = false

    val mutable rect : Widget.rect option = None
    val mutable min = 0.
    val mutable max = 100.
    val mutable step = 0.
    val mutable value = 0.
    val mutable disabled = false
    val mutable prevent_focus_state = false
    val mutable update_ui_frame = None

    method! init () : unit =
      super#init ();
      (* Register event handlers *)
      let down =
        Widget.Event.(
          let pointerdown = make "pointerdown" in
          let handler f = fun e -> self#handle_down (f e) in
          [ super#listen_lwt mousedown (handler wrap_mouse)
          ; super#listen_lwt touchstart (handler wrap_touch)
          ; super#listen_lwt pointerdown (handler wrap_mouse) ]) in
      let down' =
        let pointerdown = Widget.Event.make "pointerdown" in
        let handler _ _ =
          handling_thumb_target_evt <- true;
          Lwt.return_unit in
        match thumb_container with
        | None -> []
        | Some (elt : Element.t) ->
           Lwt_js_events.(
            [ mousedowns elt handler
            ; touchstarts elt handler
            ; seq_loop (make_event pointerdown) elt handler ]) in
      let keydown =
        super#listen_lwt Widget.Event.keydown self#handle_keydown in
      let focus =
        super#listen_lwt Widget.Event.focus self#handle_focus in
      let blur =
        super#listen_lwt Widget.Event.blur self#handle_blur in
      let resize =
        Lwt_js_events.onresizes (fun _ _ ->
            self#layout ();
            Lwt.return_unit) in
      down_handlers <- down @ down';
      keydown_handler <- Some keydown;
      focus_handler <- Some focus;
      blur_handler <- Some blur;
      resize_handler <- Some resize;
      self#layout ();
      if self#discrete && self#step =. 0.
      then step <- 1.

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      let min' =
        Option.get_or ~default:min
        @@ Option.flat_map float_of_string_opt
        @@ super#get_attribute Attr.min in
      let max' =
        Option.get_or ~default:max
        @@ Option.flat_map float_of_string_opt
        @@ Element.get_attribute elt Attr.max in
      if min' >=. self#max
      then (self#set_max max'; self#set_min min')
      else (self#set_min min'; self#set_max max');
      disabled <- (match super#get_attribute Attr.disabled with
                   | Some "true" -> true
                   | _ -> false);
      step <- (Option.get_or ~default:step
               @@ Option.flat_map float_of_string_opt
               @@ Element.get_attribute elt Attr.step);
      value <- (Option.get_or ~default:value
                @@ Option.flat_map float_of_string_opt
                @@ super#get_attribute Attr.now);
      self#setup_track_marker ()

    method! destroy () : unit =
      super#destroy ();
      (* Stop reactive events *)
      React.E.stop ~strong:true e_input;
      React.E.stop ~strong:true e_change;
      Option.iter (React.S.stop ~strong:true) s_input;
      s_input <- None;
      (* Detach event handles *)
      List.iter Lwt.cancel down_handlers;
      Option.iter Lwt.cancel keydown_handler;
      Option.iter Lwt.cancel focus_handler;
      Option.iter Lwt.cancel blur_handler;
      Option.iter Lwt.cancel resize_handler;
      down_handlers <- [];
      keydown_handler <- None;
      focus_handler <- None;
      blur_handler <- None;
      resize_handler <- None;

    method! layout () : unit =
      super#layout ();
      rect <- Some super#bounding_client_rect;
      self#update_ui_for_current_value ()

    method s_input : float React.signal =
      match s_input with
      | Some s -> s
      | None ->
         let s = React.S.hold ~eq:Float.equal self#value e_input in
         s_input <- Some s;
         s

    method e_input : float React.event =
      e_input

    method e_change : float React.event =
      e_change

    method discrete : bool =
      super#has_class Markup.CSS.discrete

    method has_track_marker : bool =
      super#has_class Markup.CSS.display_markers

    method value : float =
      value

    method set_value (v : float) : unit =
      self#set_value_ ~fire_input:false v

    method max : float =
      max

    method set_max (v : float) : unit =
      if v <. self#min
      then raise (Invalid_argument "Max cannot be less then min")
      else (
        max <- v;
        self#set_value_ ~fire_input:false ~force:true self#value;
        super#set_attribute Attr.max (string_of_float v);
        self#setup_track_marker ())

    method min : float =
      min

    method set_min (v : float) : unit =
      if v >. self#max
      then raise (Invalid_argument "Min cannot be greater then max")
      else (
        min <- v;
        self#set_value_ ~fire_input:false ~force:true self#value;
        super#set_attribute Attr.min (string_of_float v);
        self#setup_track_marker ())

    method step : float =
      step

    method set_step (v : float) : unit =
      if v <. 0.
      then raise (Invalid_argument "Step cannot be negative")
      else (
        let v = if self#discrete && v <. 1. then 1. else v in
        step <- v;
        super#set_attribute Attr.step (string_of_float v);
        self#set_value_ ~fire_input:false ~force:true self#value;
        self#setup_track_marker ())

    method disabled : bool =
      disabled

    method set_disabled (x : bool) : unit =
      disabled <- x;
      super#toggle_class ~force:x Markup.CSS.disabled;
      if x
      then (
        saved_tab_index <- Some super#tab_index;
        super#set_attribute Attr.disabled "true";
        super#remove_attribute "tabindex")
      else (
        super#remove_attribute Attr.disabled;
        Option.iter super#set_tab_index saved_tab_index)

    method step_up ?amount () =
      let amount = match amount with
        | Some x -> x
        | None -> if self#step =. 0. then 1. else self#step in
      self#set_value (self#value +. amount)

    method step_down ?amount () =
      let amount = match amount with
        | Some x -> x
        | None -> if self#step =. 0. then 1. else self#step in
      self#set_value (self#value +. amount)

    (* Private methods *)

    method private handle_down (e : event) _ : unit Lwt.t =
      let typ = Js.to_string (coerce_event e)##._type in
      match self#disabled, move_handler with
      | true, _ | _, Some _ -> Lwt.return_unit
      | false, None ->
         prevent_focus_state <- true;
         self#set_in_transit (not handling_thumb_target_evt);
         handling_thumb_target_evt <- false;
         self#set_active true;
         let body = Dom_html.document##.body in
         let up_handler _ _ =
           self#handle_up ();
           Option.iter Dom_events.stop_listen move_handler;
           move_handler <- None;
           List.iter Dom_events.stop_listen up_handlers;
           up_handlers <- [];
           true in
         let listen t h = Dom_events.listen body t h in
         let mouseup, touchend, pointerup =
           Dom_events.Typ.(mouseup, touchend, make "pointerup") in
         let move =
           let typ = List.Assoc.get_exn ~eq:String.equal typ move_event_map in
           let handler f _ e = self#handle_move (f e); true in
           Dom_events.Typ.(
             match e with
             | Mouse _ -> listen (make typ) (handler wrap_mouse)
             | Touch _ -> listen (make typ) (handler wrap_touch)) in
         let up =
           [ listen mouseup up_handler
           ; listen touchend up_handler
           ; listen pointerup up_handler
           ] in
         move_handler <- Some move;
         up_handlers <- up;
         self#set_value_from_event e;
         Lwt.return_unit

    method private handle_move (e : event) : unit =
      Dom.preventDefault (coerce_event e);
      self#set_value_from_event e

    method private handle_up () : unit =
      self#set_active false;
      self#notify_change ()

    method private handle_keydown (e : Dom_html.keyboardEvent Js.t) _ : unit Lwt.t =
      let key = Utils.Keyboard_event.event_to_key e in
      let min, max, step, value = self#min, self#max, self#step, self#value in
      let delta = match step with
        | 0. -> (max -. min) /. 100.
        | x -> x in
      let delta = match key, super#is_rtl () with
        | `Arrow_left, true | `Arrow_right, true -> -.delta
        | _ -> delta in
      let value = match key with
        | `Arrow_left | `Arrow_down -> Some (value -. delta)
        | `Arrow_right | `Arrow_up -> Some (value +. delta)
        | `Home -> Some min
        | `End -> Some max
        | `Page_up -> Some (value +. delta *. page_factor)
        | `Page_down -> Some (value -. delta *. page_factor)
        | _ -> None in
      match value with
      | None -> Lwt.return_unit
      | Some value ->
         Dom.preventDefault e;
         super#add_class Markup.CSS.focus;
         self#set_value_ ~fire_input:true value;
         self#notify_change ();
         Lwt.return_unit

    method private handle_focus _ _ : unit Lwt.t =
      if not prevent_focus_state
      then super#add_class Markup.CSS.focus;
      Lwt.return_unit

    method private handle_blur _ _ : unit Lwt.t =
      prevent_focus_state <- false;
      super#remove_class Markup.CSS.focus;
      Lwt.return_unit

    method private set_value_from_event (e : event) : unit =
      let page_x = match e with
        | Mouse e ->
           let err = "no pageX in mouse event" in
           float_of_int @@ Js.Optdef.get e##.pageX (fun () -> failwith err)
        | Touch e ->
           match Js.Optdef.to_option (e##.targetTouches##item 0) with
           | None -> failwith "No touches in touch event"
           | Some t -> float_of_int t##.pageX in
      let max, min, rect = self#max, self#min, Option.get_exn rect in
      let x = page_x -. rect.left in
      let pct = x /. (Option.get_exn rect.width) in
      let pct = if super#is_rtl () then 1. -. pct else pct in
      let value = min +. pct *. (max -. min) in
      self#set_value_ ~fire_input:true value

    method private remove_track_markers () : unit =
      match track_marker_container with
      | None -> ()
      | Some elt -> Element.remove_children elt

    method private append_track_markers (markers : int) : unit =
      match track_marker_container with
      | None -> ()
      | Some container ->
         let frag = Dom_html.document##createDocumentFragment in
         let rec loop = function
           | 0 -> ()
           | i ->
              let marker = Dom_html.(createDiv document) in
              marker##.classList##add (Js.string Markup.CSS.track_marker);
              Dom.appendChild frag marker;
              loop (pred i) in
         loop markers;
         Dom.appendChild container frag

    method private update_ui_for_current_value () : unit =
      let min, max, value, width =
        self#min, self#max, self#value,
        match Option.flat_map (fun (x : Widget.rect) -> x.width) rect with
        | None -> 0.
        | Some x -> x in
      let pct_complete = (value -. min) /. (max -. min) in
      let translate_px = pct_complete *. width in
      let translate_px =
        if super#is_rtl ()
        then width -. translate_px
        else translate_px in
      let transform =
        Utils.Animation.get_correct_property_name
          ~window:Dom_html.window "transform" in
      let transitionend =
        Utils.Animation.get_correct_event_name
          ~window:Dom_html.window "transitionend" in
      if in_transit
      then begin match thumb_container with
           | None -> ()
           | Some (elt : Element.t) ->
              let on_transitionend _ _ =
                self#set_in_transit false;
                Option.iter Dom_events.stop_listen transitionend_handler;
                transitionend_handler <- None;
                true in
              let handler =
                Dom_events.listen elt
                  (Widget.Event.make transitionend)
                  on_transitionend in
              transitionend_handler <- Some handler
           end;
      let frame =
        Utils.Animation.request_animation_frame (fun _ ->
            begin match thumb_container with
            | None -> ()
            | Some (elt : Element.t) ->
               translate_px
               |> Printf.sprintf "translateX(%gpx) translateX(-50%%)"
               |> Element.set_style_property elt transform
            end;
            begin match track with
            | None -> ()
            | Some (elt : Element.t) ->
               pct_complete
               |> Printf.sprintf "scaleX(%g)"
               |> Element.set_style_property elt transform
            end) in
      update_ui_frame <- Some frame

    method private notify_change () : unit =
      (* FIXME implement DOM event *)
      set_change self#value;
      ()

    method private notify_input () : unit =
      (* FIXME implement DOM event *)
      set_input self#value;
      ()

    method private set_marker_value (v : float) : unit =
      match pin_value_marker with
      | None -> ()
      | Some (elt : Element.t) ->
         elt##.textContent := Js.some (Js.string (string_of_float v))

    method private set_in_transit (x : bool) : unit =
      in_transit <- x;
      super#toggle_class ~force:x Markup.CSS.in_transit

    method private setup_track_marker () : unit =
      let step = self#step in
      if self#discrete && self#has_track_marker && step <>. 0.
      then (
        let markers' = (self#max -. self#min) /. step in
        let markers = ceil markers' in
        self#remove_track_markers ();
        self#append_track_markers (int_of_float markers);
        if markers' <>. markers
        then
          let last_step_ratio =
            string_of_float
            @@ (max -. (markers *. step)) /. step +. 1. in
          let flex =
            Utils.Animation.get_correct_property_name
              ~window:Dom_html.window "flex" in
          Element.query_selector super#root
            ("." ^ Markup.CSS.track_marker ^ ":last-child")
          |> function
            | None -> ()
            | Some elt -> Element.set_style_property elt flex last_step_ratio)

    method private set_active (x : bool) : unit =
      active <- x;
      super#toggle_class ~force:x Markup.CSS.active

    method private set_value_ ?(force = false) ?(fire_input = false)
                     (v : float) : unit =
      Printf.printf "setting value: %g\n" v;
      if force || not (Float.equal v self#value)
      then
        let min, max, step = self#min, self#max, self#step in
        let is_boundary = v =. min || v =. max in
        let v =
          if is_boundary || Float.equal step 0. then v
          else quantize ~step v in
        let v = if v <. min then min else if v >. max then max else v in
        Printf.printf "updated value: %g. min is %g\n" v min;
        value <- v;
        super#set_attribute Attr.now (string_of_float v);
        self#update_ui_for_current_value ();
        if fire_input
        then (self#notify_input ();
              if self#discrete then self#set_marker_value v)

  end

(** Create new slider from scratch *)
let make ?classes ?discrete ?markers ?disabled
      ?label ?step ?min ?max ?value () : t =
  let (elt : Element.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?classes ?discrete ?markers ?disabled ?label
         ?step ?min ?max ?value () in
  new t elt ()

(** Attach slider widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t =
  new t elt ()
