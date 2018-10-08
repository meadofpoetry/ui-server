open Containers
open Tyxml_js
open Utils.Keyboard_event

module Markup = Components_markup.Tab_bar.Make(Xml)(Svg)(Html)

type align = Tab_scroller.align

let ( % ) = Fun.( % )

let eq = Widget.equal

class ['a, 'b] t ?(use_auto_activation = true)
        ?on_change
        ?align
        ~(tabs : ('a, 'b) Tab.t list)
        () =
  let scroller = new Tab_scroller.t ?on_change ?align ~tabs () in
  let elt = Markup.create ~scroller:(Widget.to_markup scroller) ()
            |> To_dom.of_element in

  object(self)

    inherit Widget.t elt ()

    val mutable _use_auto_activation = use_auto_activation

    method align : align option =
      self#scroller#align

    method set_align (x : align option) : unit =
      self#scroller#set_align x

    (* Misc tab actions *)

    method tabs = scroller#tabs

    method get_tab_at_index (i : int) : ('a, 'b) Tab.t option =
      List.get_at_idx i self#tabs

    (* Active tab actions *)

    method s_active_tab : ('a, 'b) Tab.t option React.signal =
      self#scroller#s_active_tab

    method active_tab : ('a, 'b) Tab.t option =
      self#scroller#active_tab

    method active_tab_index : int option =
      match self#active_tab with
      | None -> None
      | Some x -> List.find_idx (eq x) self#tabs
                  |> Option.map fst

    method active_tab_value : 'b option =
      Option.map (fun x -> x#value) @@ self#active_tab

    method set_active_tab (tab : ('a, 'b) Tab.t) : unit =
      match List.find_opt (eq tab) self#tabs with
      | None -> ()
      | Some tab ->
         tab#set_active ?previous:self#active_tab true;
         self#scroller#set_active_tab tab;
    (* self#scroll_tab_into_view tab; *)

    method set_active_tab_index (i : int) : unit =
      Option.iter self#set_active_tab (List.get_at_idx i self#tabs)

    (* Remove tab actions *)

    method remove_tab (tab : ('a, 'b) Tab.t) : unit =
      match List.find_opt (eq tab) self#tabs with
      | None -> ()
      | Some tab ->
         self#scroller#remove_tab tab;
         if tab#active then self#set_active_tab_index 0;
         tab#destroy ()

    method remove_tab_at_idx (i : int) : unit =
      Option.iter self#remove_tab (List.get_at_idx i self#tabs)

    (* Add tab actions *)

    method append_tab (tab : ('a, 'b) Tab.t) : unit =
      self#scroller#append_tab tab;
      self#add_tab_click_listener tab

    method insert_tab_at_index (i : int) (tab : ('a,'b) Tab.t) : unit =
      self#scroller#insert_tab_at_index i tab;
      self#add_tab_click_listener tab

    (* Private methods *)

    method private scroller : ('a, 'b) Tab_scroller.t =
      scroller

    method private add_tab_click_listener (tab : ('a, 'b) Tab.t) : unit =
      tab#add_click_listener (fun _ _ ->
          if not @@ (Equal.option eq) (Some tab) self#active_tab
          then self#set_active_tab tab;
          Lwt.return_unit)

    method private get_current_translate_x () =
      let style = Dom_html.window##getComputedStyle self#scroller#content#root in
      let value = Js.to_string style##.transform in
      match value with
      | "none" -> 0
      | _ -> 0

    method private get_scroll_position () =
      let cur_translate_x = self#get_current_translate_x () in
      let scroll_left = self#scroller#area#scroll_left in
      scroll_left - cur_translate_x

    method private scroll_to next =
      let old = self#get_scroll_position () in
      Utils.Animation.animate
        ~timing:Utils.Animation.Timing.in_out_sine
        ~draw:(fun x ->
          let n = float_of_int next in
          let o = float_of_int old in
          let v = int_of_float @@ (x *. (n -. o)) +. o in
          self#scroller#area#set_scroll_left v)
        ~duration:0.35

    method private scroll_into_view (i : int) : unit =
      Option.iter self#scroll_tab_into_view @@ self#get_tab_at_index i

    method private scroll_tab_into_view (tab : ('a, 'b) Tab.t) : unit =
      let left = self#scroller#area#scroll_left in
      let width = self#scroller#area#client_width in
      let right = left + width in
      if tab#left < left
      then self#scroll_to tab#left
      else if tab#left + tab#width > right
      then self#scroll_to @@ left + (tab#width + tab#left - right)

    method private determine_target_from_key (index : int)
                     (event : key_name) : int option =
      let max_index = (List.length self#tabs) - 1 in
      let index = match event with
        | `End -> Some max_index
        | `Arrow_left -> Some (index - 1)
        | `Arrow_right -> Some (index + 1)
        | `Home -> Some 0
        | _ -> None in
      Option.map (fun x ->
          if x < 0 then max_index
          else if x > max_index then 0
          else x) index

    method private handle_key_down (e : Dom_html.keyboardEvent Js.t) : unit =
      match Utils.Keyboard_event.event_to_key e with
      | `Unknown -> ()
      | key ->
         if not @@ self#is_activation_key key
         then Dom.preventDefault e;
         let origin =
           Option.get_or
             ~default:0
             self#active_tab_index in
         if _use_auto_activation
         then
           if self#is_activation_key key then () else
             let index = self#determine_target_from_key origin key in
             Option.iter self#set_active_tab_index index
         else
           begin match self#get_focused_tab_index () with
           | None -> ()
           | Some focused_tab_index ->
              if self#is_activation_key key
              then self#set_active_tab_index focused_tab_index
              else
                let index =
                  self#determine_target_from_key focused_tab_index key in
                Option.iter self#focus_tab_at_index index;
                Option.iter self#scroll_into_view index
           end

    method private is_activation_key : key_name -> bool = function
      | `Space | `Enter -> true
      | _ -> false

    method get_focused_tab_index () : int option =
      let active_elt = Dom_html.document##.activeElement in
      match Js.Opt.to_option active_elt with
      | None -> None
      | Some a ->
         List.find_idx (fun x -> Equal.physical x#root a) self#tabs
         |> Option.map fst

    method private focus_tab_at_index (i : int) : unit =
      Option.iter (fun tab -> tab#focus ()) @@ self#get_tab_at_index i

    initializer
      Option.iter self#set_active_tab @@ List.head_opt self#tabs;
      React.S.diff (fun _ o -> o) self#s_active_tab
      |> React.E.fmap Fun.id
      |> React.E.map (fun x -> x#set_active ?previous:self#active_tab false)
      |> self#_keep_e;
      List.iter self#add_tab_click_listener self#tabs;
      self#listen_lwt Widget.Event.keydown (fun e _ ->
          self#handle_key_down e;
          Lwt.return_unit) |> Lwt.ignore_result

  end
