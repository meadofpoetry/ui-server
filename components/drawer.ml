open Containers
open Tyxml_js

module Markup = Components_markup.Drawer.Make(Xml)(Svg)(Html)

type anchor = [ `Left | `Right | `Top | `Bottom ]

class type mdc =
  object
    method open_ : bool Js.t Js.prop
  end

let open_event : Dom_html.event Js.t Dom.Event.typ  = Dom_events.Typ.make "MDCTemporaryDrawer:open"
let close_event : Dom_html.event Js.t Dom.Event.typ = Dom_events.Typ.make "MDCTemporaryDrawer:close"
let timeout ~f ~timer = Dom_html.window##setTimeout (Js.wrap_callback f) timer

class t ?(animating=true) ~(anchor:anchor) ~(content:#Widget.widget list) () =
  let drawer = Markup.create_drawer ~content:(List.map Widget.widget_to_markup content) () in
  let elt    = Markup.create ~drawer () |> Tyxml_js.To_dom.of_element in
  let e,push = React.E.create () in
  object(self)
    val mutable _anchor  = anchor
    val mutable cancel_l = None
    val mutable end_l    = None
    val _s = React.S.hold false e
    val _drawer = Widget.create @@ Tyxml_js.To_dom.of_element drawer
    inherit Widget.widget elt ()

    method anchor = _anchor
    method set_anchor (x:anchor) =
      let open Markup in
      _anchor <- x;
      match x with
      | `Left   -> _drawer#add_class anchor_left_class;
                   List.iter _drawer#remove_class [anchor_top_class; anchor_right_class; anchor_bottom_class]
      | `Top    -> _drawer#add_class anchor_top_class;
                   List.iter _drawer#remove_class [anchor_left_class; anchor_right_class; anchor_bottom_class]
      | `Right  -> _drawer#add_class anchor_right_class;
                   List.iter _drawer#remove_class [anchor_left_class; anchor_top_class; anchor_bottom_class]
      | `Bottom -> _drawer#add_class anchor_bottom_class;
                   List.iter _drawer#remove_class [anchor_left_class; anchor_top_class; anchor_right_class]

    method drawer     = _drawer
    method show ()    = self#root##.classList##add (Js.string Markup.animating_class);
                        self#root##.classList##add (Js.string Markup.open_class);
                        self#_disable_scroll ();
                        let _ = timeout ~f:(fun _ -> self#root##.classList##remove
                                                       (Js.string Markup.animating_class))
                                  ~timer:200.
                        in
                        ()

    method show_await () =
      let t,w = Lwt.wait () in
      self#show ();
      let l = ref None in
      l := Some (Dom_events.listen self#root close_event (fun _ _ ->
                                     Lwt.wakeup w ();
                                     Option.iter (fun x -> Dom_events.stop_listen x) !l;
                                     true));
      t
    method hide () = self#add_class Markup.animating_class;
                     self#remove_class Markup.open_class;
                     self#_enable_scroll ();
                     let _ = timeout
                               ~f:(fun () -> self#remove_class Markup.animating_class)
                               ~timer:200.
                     in
                     ()

    method s_state : bool React.signal = _s

    method private _disable_scroll () =
      Dom_html.document##.body##.classList##add (Js.string Markup.scroll_lock_class)
    method private _enable_scroll () =
      Dom_html.document##.body##.classList##remove (Js.string Markup.scroll_lock_class)

    method private get_delta ~x ~y ~touch = match self#anchor with
      | `Left   -> x - touch##.clientX
      | `Right  -> touch##.clientX - x
      | `Top    -> y - touch##.clientY
      | `Bottom -> touch##.clientY - y

    initializer
      Dom_events.listen self#root open_event  (fun _ _ -> push true;  true) |> ignore;
      Dom_events.listen self#root close_event (fun _ _ -> push false; true) |> ignore;

      Dom_events.listen self#root Dom_events.Typ.touchstart (fun _ ev ->
          let touch  = Js.Optdef.get (ev##.changedTouches##item 0) (fun () -> failwith "touch fail") in
          let target = Js.Opt.get (ev##.target) (fun () -> failwith "touch fail") in
          let is_anchor x = CCEqual.physical self#anchor x in
          if self#has_class Markup.open_class
             && not ((is_anchor `Top ||is_anchor `Bottom) &&
                       target##.scrollHeight > target##.offsetHeight)
             && not ((is_anchor `Right || is_anchor `Left) &&
                       target##.scrollWidth > target##.offsetWidth)
          then
            ( let start_x, start_y = touch##.clientX, touch##.clientY in
              let move_l =
                Dom_events.listen self#root Dom_events.Typ.touchmove
                  (fun _ e ->
                    Dom_html.stopPropagation e;
                    let touch = Js.Optdef.get (e##.changedTouches##item 0)
                                  (fun () -> failwith "touch fail")
                    in
                    let delta, transform =
                      (match self#anchor with
                       | `Left   -> let dx = start_x - touch##.clientX in
                                    dx, "translateX(-"^(string_of_int dx)^"px)"
                       | `Right  -> let dx = touch##.clientX - start_x in
                                    dx, "translateX(" ^(string_of_int dx)^"px)"
                       | `Top    -> let dy = start_y - touch##.clientY in
                                    dy, "translateY(-"^(string_of_int dy)^"px)"
                       | `Bottom -> let dy = touch##.clientY - start_y in
                                    dy, "translateY(" ^(string_of_int dy)^"px)")
                    in
                    if delta > 0
                    then self#drawer#style##.transform := Js.string transform;
                    true)
              in
              let end_ev e = Dom_events.stop_listen move_l;
                             Option.iter (fun x -> Dom_events.stop_listen x) end_l;
                             Option.iter (fun x -> Dom_events.stop_listen x) cancel_l;
                             let touch = Js.Optdef.get (e##.changedTouches##item 0)
                                           (fun () -> failwith "touch fail")
                             in
                             let delta = self#get_delta ~x:start_x ~y:start_y ~touch in
                             if delta > self#drawer#offset_width / 2
                             then ( self#hide (); push false;
                                    self#drawer#style##.transform := Js.string "")
                             else self#drawer#style##.transform := Js.string ""
              in
              Dom_events.listen Dom_html.window Dom_events.Typ.touchend
                (fun _ e -> end_ev e; true)
              |> (fun x -> end_l <- Some x);
              Dom_events.listen Dom_html.window Dom_events.Typ.touchcancel
                (fun _ e -> end_ev e; true)
              |> (fun x -> cancel_l <- Some x));
          true) |> ignore;

      Utils.Keyboard_event.listen Dom_html.window (function
          | `Escape _ -> if self#has_class Markup.open_class
                         then self#hide (); push false; true
          | _         -> true) |> ignore;

      Dom_events.listen self#root Dom_events.Typ.click (fun _ ev ->
          let target = Js.Opt.get (ev##.target) (fun () -> failwith "touch fail") in
          if self#has_class Markup.open_class
             && Js.to_bool @@ target##.classList##contains (Js.string Markup.base_class)
          then (self#hide (); push false); true) |> ignore;

      self#add_or_remove_class animating Markup.animating_class;
      self#set_anchor anchor
  end
