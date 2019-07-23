open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Utils

(* TODO
   - add rtl support
 *)

let ( >>= ) = Lwt.( >>= )

include Components_tyxml.Tab_bar
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

class t ?on_change ?scroller ?(auto_activation = false)
    (elt : Dom_html.element Js.t) () =
object(self)
  val _scroller = match scroller with
    | Some x -> x
    | None ->
       (* If we're attaching to an element, instantiate scroller *)
       Tab_scroller.attach @@ find_element_by_class_exn elt Tab_scroller.CSS.root
  val mutable _auto_activation = auto_activation
  val mutable _keydown_listener = None
  val mutable _interaction_listener = None

  inherit Widget.t elt () as super

  method! init () : unit =
    super#init ();
    (* Set active tab *)
    Lwt.ignore_result
    @@ (match _scroller#active_tab with
        | Some (tab : Tab.t) -> self#scroll_into_view tab
        | None ->
           match _scroller#get_tab_at_index 0 with
           | None -> Lwt.return_unit
           | Some x -> self#set_active_tab x);
    (* Attach event listeners *)
    let interaction = Tab.Event.interacts super#root self#handle_tab_interaction in
    _interaction_listener <- Some interaction;
    let keydown = Events.keydowns super#root self#handle_key_down in
    _keydown_listener <- Some keydown

  method auto_activation : bool =
    _auto_activation

  method set_auto_activation (x : bool) : unit =
    _auto_activation <- x

  method align : Tab_scroller.align option =
    _scroller#align

  method set_align (x : Tab_scroller.align option) : unit =
    _scroller#set_align x

  (* Misc tab actions *)

  method tabs : Tab.t list =
    List.sort (fun (a : Tab.t as 'a) (b : 'a) ->
        compare a#index b#index) _scroller#tabs

  method get_tab_at_index (i : int) : Tab.t option =
    _scroller#get_tab_at_index i

  (* Active tab actions *)

  method active_tab : Tab.t option =
    _scroller#active_tab

  method active_tab_index : int option =
    match _scroller#active_tab with
    | None -> None
    | Some x -> Some x#index

  method set_active_tab (tab : Tab.t) : unit Lwt.t =
    if not tab#active then (
      let eq = Option.equal ~eq:Widget.equal in
      let previous = _scroller#active_tab in
      if not @@ eq (Some tab) previous
      then (
        _scroller#set_active_tab tab;
        self#scroll_into_view tab
        >>= fun () ->
        (match on_change with
         | None -> Lwt.return_unit
         | Some f -> f previous (self :> t)))
      else Lwt.return_unit)
    else Lwt.return_unit

  method set_active_tab_index (i : int) : unit Lwt.t =
    match _scroller#get_tab_at_index i with
    | None -> Lwt.return_unit
    | Some tab -> self#set_active_tab tab

  (* Remove tab actions *)

  method remove_tab ?(destroy = true) ?(activate_other = true)
           (tab : Tab.t) : unit =
    match List.find_opt (Widget.equal tab) _scroller#tabs with
    | None -> ()
    | Some tab ->
       let i = tab#index in
       if activate_other && tab#active then (
         (* Look for previous tab *)
         let other = match _scroller#get_tab_at_index (i - 1) with
           | None -> _scroller#get_tab_at_index (i + 1) (* Try next tab *)
           | Some x -> Some x in
         Option.iter (Lwt.ignore_result % self#set_active_tab) other);
       _scroller#remove_tab tab;
       if destroy then tab#destroy ()

  method remove_tab_at_idx ?destroy ?activate_other (i : int) : unit =
    match _scroller#get_tab_at_index i with
    | None -> ()
    | Some tab -> self#remove_tab ?destroy ?activate_other tab

  (* Add tab actions *)

  method append_tab (tab : Tab.t) : unit =
    _scroller#append_tab tab

  method insert_tab_at_index (i : int) (tab : Tab.t) : unit =
    _scroller#insert_tab_at_index i tab

  (* Private methods *)

  method private is_index_in_range (i : int) : bool =
    i >= 0 && i < (List.length _scroller#tabs)

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
  method private calculate_scroll_increment (tab : Tab.t)
                   (next_index : int)
                   (scroll_position : int)
                   (bar_width : int) : int =
    let extra_scroll_amount = 20 in
    let Tab.{ content_left; content_right; _ } = tab#compute_dimensions () in
    let rel_content_left = content_left - scroll_position - bar_width in
    let rel_content_right = content_right - scroll_position in
    let left_increment = rel_content_right - extra_scroll_amount in
    let right_increment = rel_content_left + extra_scroll_amount in
    if next_index < tab#index
    then min left_increment 0
    else max right_increment 0

  (* Scrolls the tab at the given index into view *)
  method private scroll_into_view (tab : Tab.t) : unit Lwt.t =
    let i = tab#index in
    match i with
    | 0 -> _scroller#scroll_to 0
    | i when i = (List.length self#tabs) - 1 ->
       _scroller#scroll_to _scroller#content_width
    | i ->
       let scroll_position = _scroller#get_scroll_position () in
       let bar_width = super#root##.offsetWidth in
       let tab_dimensions = tab#compute_dimensions () in
       let next_index =
         self#find_adjacent_tab_index_closest_to_edge i
           tab_dimensions
           scroll_position
           bar_width in
       if not @@ self#is_index_in_range next_index then Lwt.return_unit
       else
         let scroll_increment =
           self#calculate_scroll_increment tab
             next_index
             scroll_position
             bar_width in
         _scroller#increment_scroll scroll_increment

  (* method for determining the index of the destination tab
   * based on what key was pressed
   *)
  method private determine_target_from_key
      (index : int)
      (event : Dom_html.Keyboard_code.t) : int option =
    let max_index = (List.length _scroller#tabs) - 1 in
    let index = match event with
      | End -> Some max_index
      | ArrowLeft -> Some (index - 1)
      | ArrowRight -> Some (index + 1)
      | Home -> Some 0
      | _ -> None in
    Option.map (fun x ->
        if x < 0 then max_index
        else if x > max_index then 0
        else x) index

  method private handle_tab_interaction (e : Tab.Event.interact Js.t)
      (_ : unit Lwt.t) : unit Lwt.t =
    match Js.Opt.to_option e##.detail with
    | None -> Lwt.return_unit
    | Some (elt : Element.t) ->
      List.find_opt (fun tab -> Element.equal tab#root elt) self#tabs
      |> function
      | None -> Lwt.return_unit
      | Some tab -> self#set_active_tab tab

  method private handle_key_down (e : Dom_html.keyboardEvent Js.t)
                   (_ : unit Lwt.t) : unit Lwt.t =
    match Dom_html.Keyboard_code.of_event e with
    | (ArrowLeft | ArrowRight | End | Home | Enter | Space) as key ->
       (* Prevent default behaviour for movement keys, but not for
          activation keys, since :active is used to apply ripple. *)
       if not @@ self#is_activation_key key
       then Dom.preventDefault e;
       let origin = match self#active_tab_index with
         | None -> 0
         | Some i -> i in
       if _auto_activation then (
         if self#is_activation_key key then Lwt.return_unit else
           (let index = self#determine_target_from_key origin key in
            match index with
            | None -> Lwt.return_unit
            | Some i -> self#set_active_tab_index i))
       else (
         match self#get_focused_tab () with
         | None -> Lwt.return_unit
         | Some (focused_tab : Tab.t) ->
            if self#is_activation_key key
            then self#set_active_tab focused_tab
            else
              (let index = self#determine_target_from_key focused_tab#index key in
               match index with
               | None -> Lwt.return_unit
               | Some i ->
                  match _scroller#get_tab_at_index i with
                  | None -> Lwt.return_unit
                  | Some tab -> tab#root##focus; self#scroll_into_view tab))
    | _ -> Lwt.return_unit

  method private is_activation_key : Dom_html.Keyboard_code.t -> bool = function
    | Space | Enter -> true
    | _ -> false

  method private get_focused_tab () : Tab.t option =
    let active_elt = Dom_html.document##.activeElement in
    match Js.Opt.to_option active_elt with
    | None -> None
    | Some a -> List.find_opt (fun x -> Element.equal x#root a) self#tabs
end

let make ?on_change ?auto_activation
      (scroller : Tab_scroller.t) : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ~scroller:(Widget.to_markup scroller) () in
  new t ?on_change ?auto_activation ~scroller elt ()

let attach ?on_change ?auto_activation (elt : Dom_html.element Js.t) : t =
  new t ?on_change ?auto_activation (Element.coerce elt) ()
