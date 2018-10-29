open Containers
open Tyxml_js
open Utils.Keyboard_event

module Markup = Components_markup.Tab_bar.Make(Xml)(Svg)(Html)

type align = Tab_scroller.align

let ( % ) = Fun.( % )

let eq = Widget.equal

class ['a, 'b] t ?(auto_activation = false)
        ?align
        ~(tabs : ('a, 'b) Tab.t list)
        () =
  let scroller = new Tab_scroller.t ?align ~tabs () in
  let elt = Markup.create ~scroller:(Widget.to_markup scroller) ()
            |> To_dom.of_element in

  object(self)

    inherit Widget.t elt () as super

    val mutable _auto_activation = auto_activation
    val mutable _keydown_listener = None
    val mutable _interaction_listener = None

    method! init () : unit =
      super#init ();
      begin match self#active_tab_index with
      | Some i -> self#scroll_into_view i
      | None -> Option.iter self#set_active_tab (List.head_opt self#tabs)
      end;
      React.S.diff (fun _ o -> o) self#s_active_tab
      |> React.E.fmap Fun.id
      |> React.E.map (fun x -> x#set_active ?previous:self#active_tab false)
      |> self#_keep_e;
      self#listen_lwt (Widget.Event.make Tab.interacted_event) (fun e _ ->
          Lwt.return @@ self#handle_tab_interaction e)
      |> (fun x -> _interaction_listener <- Some x);
      self#listen_lwt Widget.Event.keydown (fun e _ ->
          self#handle_key_down e;
          Lwt.return_unit)
      |> (fun x -> _keydown_listener <- Some x)

    method auto_activation : bool =
      _auto_activation

    method set_auto_activation (x : bool) : unit =
      _auto_activation <- x

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
      match List.find_idx (eq tab) self#tabs with
      | None -> ()
      | Some (idx, _) -> self#set_active_tab_index idx

    method set_active_tab_index (i : int) : unit =
      let tab = List.get_at_idx i self#tabs in
      match tab with
      | None -> ()
      | Some tab ->
         let eq = Equal.option Int.equal in
         if not @@ eq (Some i) self#active_tab_index
         then
           (tab#set_active ?previous:self#active_tab true;
            self#scroller#set_active_tab tab;
            self#scroll_into_view i)

    (* Remove tab actions *)

    method remove_tab (tab : ('a, 'b) Tab.t) : unit =
      match List.find_opt (eq tab) self#tabs with
      | None -> ()
      | Some tab ->
         self#scroller#remove_tab tab;
         (* FIXME should select previous *)
         if tab#active then self#set_active_tab_index 0;
         tab#destroy ()

    method remove_tab_at_idx (i : int) : unit =
      Option.iter self#remove_tab (List.get_at_idx i self#tabs)

    (* Add tab actions *)

    method append_tab (tab : ('a, 'b) Tab.t) : unit =
      self#scroller#append_tab tab

    method insert_tab_at_index (i : int) (tab : ('a,'b) Tab.t) : unit =
      self#scroller#insert_tab_at_index i tab

    (* Private methods *)

    method private scroller : ('a, 'b) Tab_scroller.t =
      scroller

    method private is_index_in_range (i : int) : bool =
      i >= 0 && i < (List.length self#tabs)

    (**
     * Tabs are laid out in the Tab Scroller like this:
     *
     *    Scroll Position
     *    +---+
     *    |   |   Bar Width
     *    |   +-----------------------------------+
     *    |   |                                   |
     *    |   V                                   V
     *    |   +-----------------------------------+
     *    V   |             Tab Scroller          |
     *    +------------+--------------+-------------------+
     *    |    Tab     |      Tab     |        Tab        |
     *    +------------+--------------+-------------------+
     *        |                                   |
     *        +-----------------------------------+
     *
     * To determine the next adjacent index, we look at the Tab root left and
     * Tab root right, both relative to the scroll position. If the Tab root
     * left is less than 0, then we know it's out of view to the left. If the
     * Tab root right minus the bar width is greater than 0, we know the Tab is
     * out of view to the right. From there, we either increment or decrement
     * the index.
     *)
    method private find_adjacent_tab_index_closest_to_edge (i : int)
                     ({ root_left
                      ; root_right
                      ; _ } : Tab.dimensions)
                     (scroll_position : int)
                     (bar_width : int) : int =
      let rel_root_left = root_left - scroll_position in
      let rel_root_right = root_right - scroll_position - bar_width in
      let rel_root_delta = rel_root_left + rel_root_right in
      let left_edge_is_closer = rel_root_left < 0 || rel_root_delta < 0 in
      let right_edge_is_closer = rel_root_right > 0 || rel_root_delta > 0 in
      if left_edge_is_closer
      then i - 1
      else if right_edge_is_closer
      then i + 1
      else -1

    (* Calculates the scroll increment that will make
     * the tab at the given index visible
     *)
    method private calculate_scroll_increment (i : int)
                     (next_index : int)
                     (scroll_position : int)
                     (bar_width : int) : int =
      let extra_scroll_amount = 20 in
      let tab = Option.get_exn @@ self#get_tab_at_index next_index in
      let Tab.{ content_left
              ; content_right
              ; _ } = tab#compute_dimensions () in
      let rel_content_left = content_left - scroll_position - bar_width in
      let rel_content_right = content_right - scroll_position in
      let left_increment = rel_content_right - extra_scroll_amount in
      let right_increment = rel_content_left + extra_scroll_amount in
      if next_index < i
      then min left_increment 0
      else max right_increment 0

    (* Scrolls the tab at the given index into view *)
    method private scroll_into_view (i : int) : unit =
      match self#get_tab_at_index i with
      | None -> ()
      | Some tab ->
         begin match i with
         | 0 -> self#scroller#scroll_to 0
         | i when i = (List.length self#tabs) - 1 ->
            self#scroller#scroll_to self#scroller#content#offset_width
         | i ->
            let scroll_position = self#scroller#get_scroll_position () in
            let bar_width = self#offset_width in
            let tab_dimensions = tab#compute_dimensions () in
            let next_index =
              self#find_adjacent_tab_index_closest_to_edge i
                tab_dimensions
                scroll_position
                bar_width in
            if not @@ self#is_index_in_range next_index then ()
            else
              let scroll_increment =
                self#calculate_scroll_increment i
                  next_index
                  scroll_position
                  bar_width
              in
              self#scroller#increment_scroll scroll_increment
        end

    (* method for determining the index of the destination tab
     * based on what key was pressed
     *)
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

    method private handle_tab_interaction (evt : Dom_html.event Js.t) : unit =
      let (elt : Dom_html.element Js.t) = (Js.Unsafe.coerce evt)##.detail in
      List.find_idx (fun tab -> Equal.physical tab#root elt) self#tabs
      |> function
        | None -> ()
        | Some (idx, _) -> self#set_active_tab_index idx

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
         if self#auto_activation
         then
           begin
             if self#is_activation_key key then () else
               (let index = self#determine_target_from_key origin key in
                Option.iter self#set_active_tab_index index)
           end
         else
           begin
             begin match self#get_focused_tab_index () with
             | None -> ()
             | Some focused_tab_index ->
                if self#is_activation_key key
                then self#set_active_tab_index focused_tab_index
                else
                  (let index =
                     self#determine_target_from_key focused_tab_index key in
                   Option.iter (fun x ->
                       self#focus_tab_at_index x;
                       self#scroll_into_view x) index)
             end
           end

    method private is_activation_key : key_name -> bool = function
      | `Space | `Enter -> true
      | _ -> false

    method private get_focused_tab_index () : int option =
      let active_elt = Dom_html.document##.activeElement in
      match Js.Opt.to_option active_elt with
      | None -> None
      | Some a ->
         List.find_idx (fun x -> Equal.physical x#root a) self#tabs
         |> Option.map fst

    method private focus_tab_at_index (i : int) : unit =
      Option.iter (fun tab -> tab#focus ()) @@ self#get_tab_at_index i

  end
