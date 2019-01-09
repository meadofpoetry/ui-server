open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Drawer.Make(Xml)(Svg)(Html)

type anchor = [`Left | `Right | `Top | `Bottom]

let get_anchor (w : #Widget.t) : anchor =
  if w#has_class Markup.anchor_left_class
  then `Left
  else if w#has_class Markup.anchor_top_class
  then `Top
  else if w#has_class Markup.anchor_right_class
  then `Right
  else if w#has_class Markup.anchor_bottom_class
  then `Bottom
  else failwith "no anchor set"

let get_target (e : #Dom_html.event Js.t) : Dom_html.element Js.t =
  Js.Opt.get (e##.target) (fun () -> failwith "target fail")

let get_touch (e : Dom_html.touchEvent Js.t) : Dom_html.touch Js.t =
  Js.Optdef.get (e##.changedTouches##item 0)
    (fun () -> failwith "touch fail")

class base ?anchor
        ?(animating = true)
        ~(drawer : #Dom_html.element Js.t)
        (elt : #Dom_html.element Js.t)
        () =
  let state, set_state = React.S.create false in
  object(self)
    (* Initial touch point *)
    val mutable start_x = 0
    val mutable start_y = 0

    (* Touch event listeners *)
    val mutable touchstart_listener = None
    val mutable touchmove_listener = None
    val mutable touchcancel_listener = None
    val mutable touchend_listener = None

    (* Other event listeners *)
    val mutable click_listener = None
    val mutable keydown_listener = None

    val _drawer = Widget.create drawer

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      self#add_or_remove_class animating Markup.animating_class;
      let anchor = match anchor with
        | Some x -> x
        | None ->
           try get_anchor _drawer
           with _ -> `Left in
      self#set_anchor anchor;
      (* Connect event listeners *)
      self#listen_lwt Widget.Event.touchstart (fun e _ -> self#on_touchstart e)
      |> (fun x -> touchstart_listener <- Some x);
      Dom_events.listen Dom_html.window Widget.Event.keydown
        (fun _ e -> self#on_keydown e)
      |> (fun x -> keydown_listener <- Some x);
      self#listen_click_lwt (fun e _ -> self#on_click e)
      |> (fun x -> click_listener <- Some x);

    method! destroy () : unit =
      super#destroy ();
      (* Disconnect event listeners *)
      Option.iter Lwt.cancel touchstart_listener;
      touchstart_listener <- None;
      Option.iter Lwt.cancel touchmove_listener;
      touchmove_listener <- None;
      Option.iter Dom_events.stop_listen touchend_listener;
      touchend_listener <- None;
      Option.iter Dom_events.stop_listen touchcancel_listener;
      touchcancel_listener <- None;
      Option.iter Dom_events.stop_listen keydown_listener;
      keydown_listener <- None;
      Option.iter Lwt.cancel click_listener;
      click_listener <- None

    method anchor : anchor =
      get_anchor _drawer

    method set_anchor : anchor -> unit = function
      | `Left ->
         _drawer#add_class Markup.anchor_left_class;
         List.iter _drawer#remove_class
           Markup.[ anchor_top_class
                  ; anchor_right_class
                  ; anchor_bottom_class]
      | `Top ->
         _drawer#add_class Markup.anchor_top_class;
         List.iter _drawer#remove_class
           Markup.[ anchor_left_class
                  ; anchor_right_class
                  ; anchor_bottom_class]
      | `Right ->
         _drawer#add_class Markup.anchor_right_class;
         List.iter _drawer#remove_class
           Markup.[ anchor_left_class
                  ; anchor_top_class
                  ; anchor_bottom_class ]
      | `Bottom ->
         _drawer#add_class Markup.anchor_bottom_class;
         List.iter _drawer#remove_class
           Markup.[ anchor_left_class
                  ; anchor_top_class
                  ; anchor_right_class ]

    method drawer = _drawer

    method show () : unit =
      set_state true;
      self#add_class Markup.animating_class;
      self#add_class Markup.open_class;
      self#_disable_scroll ();
      let cb = fun () -> self#remove_class Markup.animating_class in
      ignore @@ Utils.set_timeout cb 200.

    method show_await () : unit Lwt.t =
      match self#state with
      | true -> Lwt.return_unit
      | false ->
         let open Lwt.Infix in
         self#show ();
         Lwt_react.E.next (React.S.changes self#s_state)
         >|= ignore

    method hide () : unit =
      set_state false;
      self#add_class Markup.animating_class;
      self#remove_class Markup.open_class;
      self#_enable_scroll ();
      let cb = fun () -> self#remove_class Markup.animating_class in
      ignore @@ Utils.set_timeout cb 200.

    method toggle () : unit =
      if self#state then self#hide () else self#show ()

    method state : bool =
      React.S.value self#s_state

    method s_state : bool React.signal = state

    (* Private methods *)

    method private _disable_scroll () =
      Dom_html.document##.body##.classList##add
        (Js.string Markup.scroll_lock_class)

    method private _enable_scroll () =
      Dom_html.document##.body##.classList##remove
        (Js.string Markup.scroll_lock_class)

    method private get_delta ~x ~y ~touch =
      match self#anchor with
      | `Left -> x - touch##.clientX
      | `Right -> touch##.clientX - x
      | `Top -> y - touch##.clientY
      | `Bottom -> touch##.clientY - y

    method private on_click (e : Dom_html.mouseEvent Js.t) : unit Lwt.t =
      let _class = Js.string Markup.base_class in
      let target = get_target e in
      let has_base_class = Js.to_bool @@ target##.classList##contains _class in
      if self#has_class Markup.open_class && has_base_class then self#hide ();
      Lwt.return_unit

    method private on_keydown (e : Dom_html.keyboardEvent Js.t) : bool =
      match Utils.Keyboard_event.event_to_key e with
      | `Escape ->
         if self#has_class Markup.open_class then self#hide ();
         true
      | _ -> true

    method private on_event_end (e : Dom_html.touchEvent Js.t) : unit =
      Option.iter Lwt.cancel touchmove_listener;
      touchmove_listener <- None;
      Option.iter Dom_events.stop_listen touchend_listener;
      touchend_listener <- None;
      Option.iter Dom_events.stop_listen touchcancel_listener;
      touchcancel_listener <- None;
      let touch =
        Js.Optdef.get
          (e##.changedTouches##item 0)
          (fun () -> failwith "touch fail") in
      let delta = self#get_delta ~x:start_x ~y:start_y ~touch in
      if delta > self#drawer#offset_width / 2 then
        begin
          self#hide ();
          self#drawer#style##.transform := Js.string ""
        end
      else self#drawer#style##.transform := Js.string ""

    method private on_touchstart (e : Dom_html.touchEvent Js.t) : unit Lwt.t =
      let touch = get_touch e in
      let target = get_target e in
      let is_anchor x = Equal.physical self#anchor x in
      if self#has_class Markup.open_class
         && not ((is_anchor `Top || is_anchor `Bottom)
                 && target##.scrollHeight > target##.offsetHeight)
         && not ((is_anchor `Right || is_anchor `Left)
                 && target##.scrollWidth > target##.offsetWidth) then
        begin
          start_x <- touch##.clientX;
          start_y <- touch##.clientY;
          self#listen_lwt Widget.Event.touchmove
            (fun e _ -> self#on_touchmove e)
          |> (fun x -> touchmove_listener <- Some x);
          Dom_events.listen Dom_html.window Widget.Event.touchend
            (fun _ e -> self#on_event_end e; true)
          |> (fun x -> touchend_listener <- Some x);
          Dom_events.listen Dom_html.window Widget.Event.touchcancel
            (fun _ e -> self#on_event_end e; true)
          |> (fun x -> touchcancel_listener <- Some x)
        end;
      Lwt.return_unit

    method private on_touchmove (e : Dom_html.touchEvent Js.t) : unit Lwt.t =
      let touch =
        Js.Optdef.get
          (e##.changedTouches##item 0)
          (fun () -> failwith "touch fail") in
      let delta, transform =
        match self#anchor with
        | `Left ->
           let dx = start_x - touch##.clientX in
           dx, "translateX(-"^(string_of_int dx)^"px)"
        | `Right ->
           let dx = touch##.clientX - start_x in
           dx, "translateX(" ^(string_of_int dx)^"px)"
        | `Top ->
           let dy = start_y - touch##.clientY in
           dy, "translateY(-"^(string_of_int dy)^"px)"
        | `Bottom ->
           let dy = touch##.clientY - start_y in
           dy, "translateY(" ^(string_of_int dy)^"px)" in
      if delta > 0 then
        begin
          Dom_html.stopPropagation e;
          self#drawer#style##.transform := Js.string transform;
        end;
      Lwt.return_unit

  end

class t ?anchor ?animating ~content () =
  let content = List.map Widget.to_markup content in
  let drawer = Markup.create_drawer ~content () in
  let elt = To_dom.of_element @@ Markup.create ~drawer () in
  object
    inherit base ?anchor ?animating ~drawer:(To_dom.of_element drawer) elt ()
  end

let make ?anchor ?animating ~(content : #Widget.t list) () : t =
  new t ?anchor ?animating ~content ()

let attach ?anchor ?animating (elt : #Dom_html.element Js.t) : t =
  let selector = Js.string ("." ^ Markup.drawer_class) in
  let drawer = match Js.Opt.to_option @@ elt##querySelector selector with
    | Some x -> x
    | None -> failwith "no internal drawer component found" in
  new base ?anchor ?animating ~drawer elt ()
