(* open Containers
 * open Tyxml_js
 * 
 * module Markup = Components_markup.Tabs.Make(Xml)(Svg)(Html)
 * 
 * module Tab_bar = struct
 * 
 *   class ['a,'b] t ~(tabs:('a,'b) tab list) (fmt:'a content) () =
 * 
 *     let s_active,s_active_push = React.S.create None in
 *     let tabs = List.map (fun x -> new Tab.t s_active_push fmt x ()) tabs in
 *     let indicator =
 *       Markup.Tab_bar.Indicator.create ()
 *       |> Tyxml_js.To_dom.of_span
 *       |> Widget.create in
 *     let elt =
 *       Markup.Tab_bar.create
 *         ~indicator:(Widget.to_markup indicator)
 *         ~tabs:(List.map Widget.to_markup tabs) ()
 *       |> Tyxml_js.To_dom.of_nav in
 *     let () = Option.iter (fun x -> x#set_active true) (List.head_opt tabs) in
 * 
 *     object(self)
 * 
 *       inherit Widget.t elt () as super
 * 
 *       val mutable init         = false
 *       val mutable tabs         = tabs
 *       val mutable layout_frame = None
 *       val mutable width        = 0
 * 
 *       method indicator_widget = indicator
 * 
 *       method get_tab_at_index i = List.get_at_idx i self#tabs
 * 
 *       method remove_tab_at_index i = match self#get_tab_at_index i with
 *         | Some tab ->
 *            tabs <- List.remove_at_idx i tabs;
 *            self#remove_child tab;
 *            if tab#active then s_active_push None;
 *            self#layout ()
 *         | None -> ()
 * 
 *       method layout () =
 *         let wnd = Dom_html.window in
 *         Option.iter (fun x -> wnd##cancelAnimationFrame x) layout_frame;
 *         let f = fun _ ->
 *           self#layout_internal ();
 *           layout_frame <- None in
 *         layout_frame <- Some (wnd##requestAnimationFrame (Js.wrap_callback f))
 * 
 *       method private layout_internal () = match self#active_tab with
 *         | Some tab ->
 *            let f () =
 *              width <- self#offset_width;
 *              let l = tab#left in
 *              let w = match width with
 *                | 0 -> 0.
 *                | x -> (float_of_int tab#width) /. (float_of_int x)
 *              in
 *              let t = Printf.sprintf "translateX(%dpx) scale(%f,1)" l w in
 *              if not init
 *              then (Js.Unsafe.coerce indicator#style)##.transition :=
 *                     Js.string "none";
 *              indicator#style##.transform := Js.string t;
 *              if not init
 *              then
 *                (indicator#offset_width |> ignore;
 *                 (Js.Unsafe.coerce indicator#style)##.transition := Js.string "";
 *                 indicator#style##.visibility := Js.string "visible";
 *                 init <- true)
 *            in
 *            (match Js.Opt.to_option @@ self#root##.parentNode with
 *             | None   ->
 *                let p_vis = self#style##.visibility in
 *                let p_pos = self#style##.position in
 *                self#style##.visibility := Js.string "hidden";
 *                self#style##.position   := Js.string "absolute";
 *                Dom.appendChild Dom_html.document##.body self#root;
 *                f ();
 *                Dom.removeChild Dom_html.document##.body self#root;
 *                self#style##.visibility := p_vis;
 *                self#style##.position   := p_pos
 *             | Some _ -> f ())
 *         | None ->
 *            (Js.Unsafe.coerce indicator#style)##.transition := Js.string "none";
 *            indicator#style##.visibility := Js.string "hidden";
 *            init <- false
 * 
 *       initializer
 *         self#add_class (Markup.Tab_bar._class ^ "-upgraded");
 *         Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ ->
 *             self#layout (); false) |> ignore;
 *         React.S.map (fun _   -> self#layout ()) s_active |> ignore;
 *         React.E.map (fun tab -> Option.iter (fun x -> x#set_active false) tab)
 *           (React.S.diff (fun _ x -> x) s_active)
 *         |> ignore
 * 
 * 
 *     end
 * 
 * end
 * 
 * module Scroller = struct
 * 
 *   class ['a,'b] t ~(tabs:('a,'b) tab list) (fmt:'a content) () =
 * 
 *     let tab_bar = new Tab_bar.t ~tabs fmt () in
 *     let elt     =
 *       Markup.Scroller.create ~tabs:(Widget.to_markup tab_bar) ()
 *       |> Tyxml_js.To_dom.of_div in
 *     let wrapper =
 *       elt##querySelector (Js.string ("." ^ Markup.Scroller.scroll_frame_tabs_class))
 *       |> Js.Opt.to_option |> Option.get_exn |> Widget.create
 *     in
 *     let back    =
 *       elt##querySelector (Js.string ("." ^ Markup.Scroller.indicator_back_class))
 *       |> Js.Opt.to_option |> Option.get_exn |> Widget.create
 *     in
 *     let forward =
 *       elt##querySelector (Js.string ("." ^ Markup.Scroller.indicator_forward_class))
 *       |> Js.Opt.to_option |> Option.get_exn |> Widget.create
 *     in
 *     let on_scroll_change = (fun _ h ->
 *         wrapper#style##.marginBottom := Js.string (Printf.sprintf "-%dpx" h)) in
 *     let scroll_listener  = new Utils.Scroll_size_listener.t
 *                              ~on_change:on_scroll_change () in
 * 
 *     object(self)
 * 
 *       inherit Widget.t elt () as super
 * 
 *       (\* User methods *\)
 * 
 *       method tab_bar           = tab_bar
 *       method scroll_back ()    = self#move_tabs_scroll (- wrapper#client_width)
 *       method scroll_forward () = self#move_tabs_scroll wrapper#client_width
 *       method layout ()         =
 *         super#layout ();
 *         self#update_scroll_buttons ();
 *         self#tab_bar#layout ();
 *         scroll_listener#measure ()
 * 
 *       (\* Private methods *\)
 * 
 *       method private scroll next =
 *         let old = wrapper#root##.scrollLeft in
 *         Utils.Animation.animate
 *           ~timing:Utils.Animation.Timing.in_out_sine
 *           ~draw:(fun x ->
 *             let n = float_of_int next in
 *             let o = float_of_int old in
 *             let v = int_of_float @@ (x *. (n -. o)) +. o in
 *             wrapper#root##.scrollLeft := v)
 *           ~duration:0.35
 * 
 *       method private move_tabs_scroll (delta:int) =
 *         let multiplier = 1 in
 *         let old        = wrapper#root##.scrollLeft in
 *         let next       = old + delta * multiplier in
 *         self#scroll next
 * 
 *       method private scroll_tab_into_view (tab:('a,'b) Tab.t) =
 *         let left  = wrapper#root##.scrollLeft in
 *         let right = left + wrapper#client_width in
 *         if tab#left < left
 *         then self#scroll tab#left
 *         else if tab#left + tab#width > right
 *         then self#scroll @@ left + (tab#width + tab#left - right)
 * 
 *       method private update_scroll_buttons () =
 *         let scroll_width = wrapper#root##.scrollWidth in
 *         let scroll_left  = wrapper#root##.scrollLeft in
 *         let client_width = wrapper#root##.clientWidth in
 *         if scroll_width <= client_width
 *         then (back#style##.display := Js.string "none"; forward#style##.display := Js.string "none")
 *         else (back#style##.display := Js.string ""; forward#style##.display := Js.string "");
 *         let show_left    = scroll_left > 0 in
 *         let show_rigth   = scroll_width > client_width + scroll_left in
 *         let f x w        = w#add_or_remove_class x Markup.Scroller.indicator_enabled_class in
 *         f show_left back;
 *         f show_rigth forward
 * 
 *       initializer
 *         self#layout ();
 *         Dom.appendChild self#root scroll_listener#root;
 *         Dom_events.(listen back#root    Typ.click  (fun _ _ -> self#scroll_back ();    true))        |> ignore;
 *         Dom_events.(listen forward#root Typ.click  (fun _ _ -> self#scroll_forward (); true))        |> ignore;
 *         Dom_events.(listen wrapper#root Typ.scroll (fun _ _ -> self#update_scroll_buttons (); true)) |> ignore;
 *         React.S.map (fun x -> Option.iter self#scroll_tab_into_view x) self#tab_bar#s_active      |> ignore;
 * 
 *     end
 * 
 * end *)
