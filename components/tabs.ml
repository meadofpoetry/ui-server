open Containers

type 'a tab =
  { href     : string option
  ; content  : content
  ; disabled : bool
  ; value    : 'a
  }
and content = [ `Text of string | `Icon of string * (string option) | `Text_and_icon of string * string ]

module Tab = struct

  class ['a] t push (props:'a tab) () =

    let elt = Markup.Tabs.Tab.create ~content:props.content () |> Tyxml_js.To_dom.of_a in

    object(self)

      inherit Widget.widget elt () as super

      val mutable tab    = props
      val mutable active = false
      val mutable prevent_default_on_click = false
      val mutable left  = 0
      val mutable width = 0
      val mutable value : 'a = props.value

      method anchor_element = elt

      method set_value (x:'a) = value <- x
      method get_value : 'a   = value

      method get_disabled   = tab.disabled
      method set_disabled x = tab <- { tab with disabled = x };
                              let a = "disabled" in
                              if x then self#set_attribute a "true" else self#remove_attribute a

      method get_href   = Js.to_string self#anchor_element##.href
      method set_href s = self#anchor_element##.href := Js.string s

      method get_content : content = tab.content
      method set_content (x : content) =
        match self#get_content,x with
        | `Text _, `Text x ->
           super#set_text_content x;
           Result.return ()
        | `Icon _, `Icon (i,fallback) ->
           let icon = super#get_child_element_by_class Markup.Tabs.Tab.icon_class in
           (match icon with
            | Some icon -> icon##.textContent := Js.some @@ Js.string i;
                           Option.iter (fun x -> icon##setAttribute (Js.string "aria-label") (Js.string x)) fallback;
                           Result.return ()
            | None      -> Result.fail "icon element not found")
        | `Text_and_icon _, `Text_and_icon (t,i) ->
           let icon = super#get_child_element_by_class Markup.Tabs.Tab.icon_class in
           let text = super#get_child_element_by_class Markup.Tabs.Tab.icon_text_class in
           (match icon,text with
            | Some icon, Some text -> icon##.textContent := Js.some @@ Js.string i;
                                      text##.textContent := Js.some @@ Js.string t;
                                      Result.return ()
            | None, _ -> Result.fail "icon element not found"
            | _, None -> Result.fail "text element not found")
        | _  -> Result.fail "tab content type mismatch"

      method get_active   = active
      method set_active x = active <- x;
                            if x
                            then (self#add_class Markup.Tabs.Tab.active_class;
                                  push (Some self))
                            else self#remove_class Markup.Tabs.Tab.active_class

      method get_width = width
      method get_left  = left

      method measure_self = width <- self#get_offset_width;
                            left  <- self#get_offset_left

      method get_prevent_default_on_click   = prevent_default_on_click
      method set_prevent_default_on_click x = prevent_default_on_click <- x

      initializer
        Ripple.attach self |> ignore;
        self#set_attribute "tabindex" "0";
        self#set_disabled tab.disabled;
        Option.iter (fun x -> self#set_href x) props.href;
        Dom_events.listen self#root Dom_events.Typ.click (fun _ _ ->
                            self#set_active true;
                            not prevent_default_on_click)
        |> ignore;
        Dom_events.listen self#root Dom_events.Typ.keydown (fun _ (ev:Dom_html.keyboardEvent Js.t) ->
                            let key  = Option.map Js.to_string @@ Js.Optdef.to_option ev##.key in
                            (match key,ev##.keyCode with
                             | Some "Enter", _ | _, 13 -> self#set_active true
                             | _ -> ());
                            true)
        |> ignore

    end

end

module Tab_bar = struct

  class ['a] t ~(tabs:'a tab list) () =

    let s_active,s_active_push = React.S.create None in
    let tabs      = List.map (fun x -> new Tab.t s_active_push x ()) tabs in
    let indicator = new Widget.widget (Markup.Tabs.Tab_bar.Indicator.create () |> Tyxml_js.To_dom.of_span) () in
    let typ_of_tab_content = function
      | `Text _          -> `Text
      | `Icon _          -> `Icon
      | `Text_and_icon _ -> `Text_and_icon
    in
    let typ = List.map (fun x -> typ_of_tab_content x#get_content) tabs
              |> List.sort_uniq ~cmp:Pervasives.compare
              |> (function
                  | [x] -> x
                  | _   -> failwith "All tabs must be of the same type: text, icon or text with icon") in
    let elt = Markup.Tabs.Tab_bar.create ~typ
                                         ~indicator:(Widget.widget_to_markup indicator)
                                         ~tabs:(Widget.widgets_to_markup tabs) ()
              |> Tyxml_js.To_dom.of_nav in
    let ()  = Option.iter (fun x -> x#set_active true) (List.head_opt tabs) in

    object(self)

      inherit Widget.widget elt () as super

      val mutable init         = false
      val mutable tabs         = tabs
      val mutable layout_frame = None
      val mutable width        = 0

      method typ : [ `Text | `Icon | `Text_and_icon ] = typ
      method indicator_widget = indicator
      method tabs             = tabs
      method s_active         = s_active


      method set_indicator_default = super#remove_class Markup.Tabs.Tab_bar.indicator_accent_class;
                                     super#remove_class Markup.Tabs.Tab_bar.indicator_primary_class
      method set_indicator_primary = super#remove_class Markup.Tabs.Tab_bar.indicator_accent_class;
                                     super#add_class Markup.Tabs.Tab_bar.indicator_primary_class
      method set_indicator_accent  = super#remove_class Markup.Tabs.Tab_bar.indicator_primary_class;
                                     super#add_class Markup.Tabs.Tab_bar.indicator_accent_class

      (* Active getters *)

      method get_active_tab_index = match React.S.value s_active with
        | Some tab -> List.find_idx (fun x -> Equal.physical x tab) self#tabs
                      |> (function Some (idx,_) -> Some idx | None -> None)
        | None     -> None
      method get_active_tab = React.S.value s_active
      method get_active_value = Option.map (fun x -> x#get_value) self#get_active_tab

      (* Active setters *)

      method set_active_tab_index x = match List.get_at_idx x self#tabs with
        | Some tab -> Ok (tab#set_active true)
        | None     -> Error (Printf.sprintf "set_active_tab_index: tab with index %d not found" x)
      method set_active_tab tab = match List.find_idx (fun x -> Equal.physical x tab) self#tabs with
        | Some (_,tab) -> Ok (tab#set_active true)
        | None         -> Error "set_active_tab: tab not found"

      method get_tab_at_index i = List.get_at_idx i self#tabs

      method append_tab (tab : 'a tab) =
        if Equal.poly (typ_of_tab_content tab.content) self#typ
        then (let t = new Tab.t s_active_push tab () in
              tabs <- tabs @ [t];
              Dom.appendChild self#root t#root;
              self#layout;
              Ok ())
        else (Error "append_tab: tab content mismatch")

      method insert_tab_at_index index (tab : 'a tab) =
        if Equal.poly (typ_of_tab_content tab.content) self#typ
        then (let t = new Tab.t s_active_push tab () in
              tabs <- List.insert_at_idx index t tabs;
              let tab_elts = self#root##.childNodes in
              let item     = tab_elts##item index in
              let _        = match Js.Opt.to_option item with
                | Some _ -> self#root##insertBefore (t#root :> Dom.node Js.t) item
                | None   -> self#root##appendChild  (t#root :> Dom.node Js.t)
              in
              self#layout;
              Ok ())
        else (Error "insert_tab_at_index: tab content mismatch")

      method remove_tab_at_index i = match self#get_tab_at_index i with
        | Some tab -> tabs <- List.remove_at_idx i tabs;
                      Dom.removeChild self#root tab#root;
                      if tab#get_active then s_active_push None;
                      self#layout;
                      Ok ()
        | None     -> Error (Printf.sprintf "remove_tab_at_index: tab with index %d not found" i)

      method private layout_internal = match self#get_active_tab with
        | Some tab ->
           let f () =
             List.iter (fun x -> x#measure_self) self#tabs;
             width <- self#get_offset_width;
             let l = tab#get_left in
             let w = match width with
               | 0 -> 0.
               | x -> (float_of_int tab#get_width) /. (float_of_int x)
             in
             let t = Printf.sprintf "translateX(%dpx) scale(%f,1)" l w in
             if not init then (Js.Unsafe.coerce indicator#style)##.transition := Js.string "none";
             indicator#style##.transform := Js.string t;
             if not init then (indicator#get_offset_width |> ignore;
                               (Js.Unsafe.coerce indicator#style)##.transition := Js.string "";
                               indicator#style##.visibility := Js.string "visible";
                               init <- true)
           in
           (match Js.Opt.to_option @@ self#root##.parentNode with
            | None   -> let p_vis = self#style##.visibility in
                        let p_pos = self#style##.position in
                        self#style##.visibility := Js.string "hidden";
                        self#style##.position   := Js.string "absolute";
                        Dom.appendChild Dom_html.document##.body self#root;
                        f ();
                        Dom.removeChild Dom_html.document##.body self#root;
                        self#style##.visibility := p_vis;
                        self#style##.position   := p_pos
            | Some _ -> f ())
        | None -> (Js.Unsafe.coerce indicator#style)##.transition := Js.string "none";
                  indicator#style##.visibility := Js.string "hidden";
                  init <- false

      method private layout =
        Option.iter (fun x -> Dom_html.window##cancelAnimationFrame x) layout_frame;
        let f = fun _ -> self#layout_internal; layout_frame <- None in
        layout_frame <- Some (Dom_html.window##requestAnimationFrame (Js.wrap_callback f))

      initializer
        self#add_class (Markup.Tabs.Tab_bar._class ^ "-upgraded");
        Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ -> self#layout; false) |> ignore;
        React.S.map (fun _   -> self#layout) s_active |> ignore;
        React.E.map (fun tab -> Option.iter (fun x -> x#set_active false) tab) (React.S.diff (fun _ x -> x) s_active)
        |> ignore


    end

end

let in_out_sine x =
  let pi = 4.0 *. atan 1.0 in
  0.5 *. (1. -. (cos (pi *. x)))

let animate ~(timing   : float -> float)
            ~(draw     : float -> unit)
            ~(duration : float) =
  let start = Unix.gettimeofday () *. 1000. in

  let rec cb = (fun time ->
      let time_fraction = Pervasives.min ((time -. start) /. duration) 1. in
      let progress      = timing time_fraction in
      let ()            = draw progress in
      let (<)           = Pervasives.(<) in

      if time_fraction < 1.
      then
        let _ = Dom_html.window##requestAnimationFrame (Js.wrap_callback cb) in
        ())
  in

  let _ = Dom_html.window##requestAnimationFrame (Js.wrap_callback cb) in
  ()

module Scroller = struct

  class type mdc =
    object
      method tabBar : unit Js.t Js.readonly_prop
    end

  class ['a] t ~(tabs:'a tab list) () =

    let tab_bar = new Tab_bar.t ~tabs () in
    let elt = (* tab_bar#add_class Markup.Tabs.Scroller.scroll_frame_tabs_class; *)
              Markup.Tabs.Scroller.create ~tabs:(Widget.widget_to_markup tab_bar) ()
              |> Tyxml_js.To_dom.of_div in

    object(self)

      inherit Widget.widget elt ()
      method tab_bar = tab_bar

      method private handle_left_scroll_click = ()
      method private handle_right_scroll_click = ()
      method private move_tabs_scroll (delta:int) =
        let multiplier = 1 in
        let next_scroll_left = self#tab_bar#root##.scrollLeft + delta * multiplier in
        animate ~timing:in_out_sine
                ~draw:(fun x -> let v = (float_of_int next_scroll_left) *. x in
                                self#root##.scrollLeft := int_of_float v)
                ~duration:0.35

      method private scroll_selected_into_view = ()

      method private handle_tabs_scroll = ()

    end

end
