open Containers
open Tyxml_js

module Markup = Components_markup.Drawer.Make(Xml)(Svg)(Html)

type anchor = [ `Left | `Right | `Top | `Bottom ]

let timeout ~f ~timer = Dom_html.window##setTimeout (Js.wrap_callback f) timer

class t ?(animating=true) ~(anchor:anchor) ~(content:#Widget.t list) () =
  let drawer = Markup.create_drawer ~content:(List.map Widget.to_markup content) () in
  let elt = Markup.create ~drawer () |> Tyxml_js.To_dom.of_element in
  let state, set_state = React.S.create false in
  object(self)
    val mutable _anchor  = anchor
    val mutable cancel_l = None
    val mutable end_l    = None
    val _drawer = Widget.create @@ Tyxml_js.To_dom.of_element drawer
    inherit Widget.t elt ()

    method anchor = _anchor
    method set_anchor (x : anchor) =
      let open Markup in
      _anchor <- x;
      match x with
      | `Left ->
         _drawer#add_class anchor_left_class;
         List.iter _drawer#remove_class
           [ anchor_top_class
           ; anchor_right_class
           ; anchor_bottom_class]
      | `Top ->
         _drawer#add_class anchor_top_class;
         List.iter _drawer#remove_class
           [ anchor_left_class
           ; anchor_right_class
           ; anchor_bottom_class]
      | `Right ->
         _drawer#add_class anchor_right_class;
         List.iter _drawer#remove_class
           [ anchor_left_class
           ; anchor_top_class
           ; anchor_bottom_class ]
      | `Bottom ->
         _drawer#add_class anchor_bottom_class;
         List.iter _drawer#remove_class
           [ anchor_left_class
           ; anchor_top_class
           ; anchor_right_class ]

    method drawer = _drawer
    method show () =
      set_state true;
      self#add_class Markup.animating_class;
      self#add_class Markup.open_class;
      self#_disable_scroll ();
      let _ =
        timeout
          ~f:(fun _ ->
              self#remove_class Markup.animating_class)
          ~timer:200. in
      ()

    method show_await () = match React.S.value self#s_state with
      | true  -> Lwt.return_unit
      | false ->
         let open Lwt.Infix in
         let t, w = Lwt.wait () in
         self#show ();
         Lwt_react.E.next (React.S.changes self#s_state)
         >|= (fun _ -> Lwt.wakeup w ())
         |> Lwt.ignore_result;
         t

    method hide () =
      set_state false;
      self#add_class Markup.animating_class;
      self#remove_class Markup.open_class;
      self#_enable_scroll ();
      let _ =
        timeout
          ~f:(fun () ->
              self#remove_class Markup.animating_class)
          ~timer:200. in
      ()

    method s_state : bool React.signal = state

    method private _disable_scroll () =
      Dom_html.document##.body##.classList##add
        (Js.string Markup.scroll_lock_class)
    method private _enable_scroll () =
      Dom_html.document##.body##.classList##remove
        (Js.string Markup.scroll_lock_class)

    method private get_delta ~x ~y ~touch =
      match self#anchor with
      | `Left   -> x - touch##.clientX
      | `Right  -> touch##.clientX - x
      | `Top    -> y - touch##.clientY
      | `Bottom -> touch##.clientY - y

    initializer

      self#listen_lwt Widget.Event.touchstart (fun e _ ->
          let touch =
            Js.Optdef.get
              (e##.changedTouches##item 0)
              (fun () -> failwith "touch fail") in
          let target =
            Js.Opt.get
              (e##.target)
              (fun () -> failwith "touch fail") in
          let is_anchor x = Equal.physical self#anchor x in
          if self#has_class Markup.open_class
          && not ((is_anchor `Top ||is_anchor `Bottom)
                  && target##.scrollHeight > target##.offsetHeight)
          && not ((is_anchor `Right || is_anchor `Left)
                  && target##.scrollWidth > target##.offsetWidth) then
            ( let start_x, start_y = touch##.clientX, touch##.clientY in
              let move_l =
                self#listen_lwt Widget.Event.touchmove
                  (fun e _ ->
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
                     Lwt.return_unit) in
              let end_ev e =
                Lwt.cancel move_l;
                Option.iter (fun x -> Dom_events.stop_listen x) end_l;
                Option.iter (fun x -> Dom_events.stop_listen x) cancel_l;
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
                else
                  self#drawer#style##.transform := Js.string ""
              in
              Dom_events.listen Dom_html.window Dom_events.Typ.touchend
                (fun _ e -> end_ev e; true)
              |> (fun x -> end_l <- Some x);
              Dom_events.listen Dom_html.window Dom_events.Typ.touchcancel
                (fun _ e -> end_ev e; true)
              |> (fun x -> cancel_l <- Some x));
          Lwt.return_unit)
      |> Lwt.ignore_result;

      Dom_events.listen Dom_html.window Dom_events.Typ.keydown
        (fun _ e ->
          match Utils.Keyboard_event.event_to_key e with
          | `Escape ->
            if self#has_class Markup.open_class then
              begin
                match self#get_attribute "tabindex" with
                | Some x ->
                  if int_of_string x >= 0 then self#hide ()
                | None   -> ();
              end;
            true
          | _ -> true)
      |> ignore;

      self#listen_click_lwt (fun e _ ->
          let target =
            Js.Opt.get
              (e##.target)
              (fun () -> failwith "touch fail") in
          if self#has_class Markup.open_class
          && Js.to_bool
             @@ target##.classList##contains
               (Js.string Markup.base_class) then
            self#hide ();
          Lwt.return_unit)
      |> Lwt.ignore_result;

      self#add_or_remove_class animating Markup.animating_class;
      self#set_anchor anchor
  end
