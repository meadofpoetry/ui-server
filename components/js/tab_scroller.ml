open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_tyxml.Tab_scroller.Make(Xml)(Svg)(Html)

let ( % ) = Fun.( % )

type align =
  | Start
  | End
  | Center

type animation =
  { final_scroll_position : int
  ; scroll_delta : int
  }

let horizontal_scroll_height = ref None

let compute_horizontal_scroll_height ?(cache = true) () : int option =
  match cache, !horizontal_scroll_height with
  | true, Some x -> Some x
  | _ ->
     let el = Dom_html.(createDiv document) in
     el##.classList##add (Js.string Markup.scroll_test_class);
     Dom.appendChild Dom_html.document##.body el;
     let height = el##.offsetHeight - el##.clientHeight in
     if cache then horizontal_scroll_height := Some height;
     Dom.removeChild Dom_html.document##.body el;
     Some height

class ['a, 'b] t ?align
        ~(tabs : ('a, 'b) Tab.t list) () =
  let eq = Widget.equal in
  let tabs' = List.map Widget.to_markup tabs in
  let content =
    Markup.create_scroll_content tabs' ()
    |> To_dom.of_element
    |> Widget.create in
  let area =
    Markup.create_scroll_area ~content:(Widget.to_markup content) ()
    |> To_dom.of_element
    |> Widget.create in
  let elt =
    Markup.create ~scroll_area:(Widget.to_markup area) ()
    |> To_dom.of_element in
  let active_tab = List.find_opt (fun tab -> tab#active) tabs in
  let s_active, set_active = React.S.create ~eq:(Equal.option eq) active_tab in
  object(self)

    val mutable _tabs : ('a, 'b) Tab.t list = tabs
    val mutable _align : align option = align
    val mutable _animating : bool = false
    val mutable _listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      self#_init ();
      area#style##.marginBottom :=
        (match compute_horizontal_scroll_height () with
         | Some x -> Js.string (Printf.sprintf "-%dpx" x)
         | None -> Js.string "");
      area#add_class Markup.scroll_area_scroll_class;
      self#set_align _align

    method! destroy () : unit =
      super#destroy ();
      List.iter Dom_events.stop_listen _listeners;
      _listeners <- []

    method s_active_tab : ('a, 'b) Tab.t option React.signal =
      s_active

    method active_tab : ('a, 'b) Tab.t option =
      React.S.value self#s_active_tab

    method set_active_tab (tab : ('a, 'b) Tab.t) : unit =
      set_active @@ Some tab;
      self#layout ()

    method remove_tab (tab : ('a, 'b) Tab.t) : unit =
      match List.find_opt (eq tab) self#tabs with
      | None -> ()
      | Some tab ->
         self#remove_child tab;
         _tabs <- List.remove ~eq tab self#tabs;
         self#layout ()

    method append_tab (tab : ('a, 'b) Tab.t) : unit =
      _tabs <- tabs @ [tab];
      self#append_child tab;
      self#layout ()

    method insert_tab_at_index (i : int) (tab : ('a, 'b) Tab.t) : unit =
      _tabs <- List.insert_at_idx i tab _tabs;
      self#insert_child_at_idx i tab;
      self#layout ()

    method align : align option = _align

    method set_align (x : align option) : unit =
      _align <- x;
      let pre = Components_tyxml.CSS.add_modifier
                  Markup.base_class "align-" in
      List.iter self#remove_class @@ self#find_classes pre;
      match x with
      | None -> ()
      | Some Start -> self#add_class Markup.align_start_class
      | Some End -> self#add_class Markup.align_end_class
      | Some Center -> self#add_class Markup.align_center_class

    method tabs = _tabs

    method area = area

    method content = content

    (* Computes the current visual scroll position *)
    method get_scroll_position () : int =
      let current_translate_x = self#get_current_translate_x () in
      let scroll_left = self#area#scroll_left in
      scroll_left - current_translate_x

    (* Scrolls to the given scrollX value *)
    method scroll_to (scroll_x : int) : unit =
      let current_scroll_x = self#get_scroll_position () in
      let safe_scroll_x = self#clamp_scroll_value scroll_x in
      let scroll_delta = safe_scroll_x - current_scroll_x in
      self#animate { scroll_delta; final_scroll_position = safe_scroll_x }

    (* Increment scroll value by the given value *)
    method increment_scroll (scroll_x : int) : unit =
      match scroll_x with
      | 0 -> ()
      | x ->
         let current_scroll_x = self#get_scroll_position () in
         let target_scroll_x = x + current_scroll_x in
         let safe_scroll_x = self#clamp_scroll_value target_scroll_x in
         let scroll_delta = safe_scroll_x - current_scroll_x in
         self#animate { scroll_delta; final_scroll_position = safe_scroll_x }

    (* Private methods *)

    (* Handles interaction events that occur during transition *)
    method private handle_interaction () : unit =
      if _animating then self#stop_scroll_animation ()

    (* Handles transitionend event *)
    method private handle_transition_end e : unit =
      let eq = Equal.option Equal.physical in
      let target = Js.Opt.to_option e##.target in
      if _animating && eq (Some self#content#root) target
      then begin
          _animating <- false;
          self#remove_class Markup.animating_class
        end

    method private calculate_scroll_edges () : int * int =
      let content_width = self#content#offset_width in
      let root_width = self#area#offset_width in
      (* left, right *)
      (0, content_width - root_width)

    (* Calculates a safe scroll value that is > 0 and < the max scroll value
     * v - the distance to scroll
     *)
    method private clamp_scroll_value (v : int) :  int =
      let left, right = self#calculate_scroll_edges () in
      min (max left v) right

    method private get_current_translate_x () =
      let style = Dom_html.window##getComputedStyle self#content#root in
      let value = Js.to_string style##.transform in
      match value with
      | "none" -> 0
      | value ->
         let tx =
           Option.map (Float.to_int % Float.of_string % String.trim)
           @@ List.get_at_idx 4
           @@ String.split ~by:"," value in
         begin match tx with
         | Some x -> x
         | None -> 0
         end

    (* Gets the current scroll position during animation *)
    method private get_animating_scroll_position () : int =
      let current_translate_x = self#get_current_translate_x () in
      let scroll_left = self#area#scroll_left in
      scroll_left - current_translate_x

    (* Stops scroll animation *)
    method private stop_scroll_animation () : unit =
      _animating <- false;
      let current_scroll_position = self#get_animating_scroll_position () in
      self#remove_class Markup.animating_class;
      self#style##.transform := Js.string "translateX(0px)";
      self#area#set_scroll_left current_scroll_position

    (* Animates the tab scrolling *)
    method private animate (a : animation) : unit =
      (* Early exit if translateX is 0, which means
       * there is no animation to perform *)
      if a.scroll_delta = 0 then () else
        begin
          let translate_x = Printf.sprintf "translateX(%dpx)" a.scroll_delta in
          self#stop_scroll_animation ();
          self#area#set_scroll_left a.final_scroll_position;
          self#content#style##.transform := Js.string translate_x;
          (* Force repaint *)
          ignore @@ self#area#bounding_client_rect;
          let wnd = Dom_html.window in
          let cb = fun _ ->
            self#add_class Markup.animating_class;
            self#content#style##.transform := Js.string "none" in
          ignore @@ wnd##requestAnimationFrame (Js.wrap_callback cb);
          _animating <- true;
        end

    method private _init () : unit =
      let open Widget.Event in
      let handler = fun _ _ -> self#handle_interaction (); true in
      let wheel = self#listen wheel handler in
      let touchstart = self#listen touchstart handler in
      let pointerdown = self#listen (make "pointerdown") handler in
      let mousedown = self#listen mousedown handler in
      let keydown = self#listen keydown handler in
      let transitionend =
        self#listen (make "transitionend") (fun _ e->
            self#handle_transition_end e;
            true) in
      let listeners =
        [ wheel
        ; touchstart
        ; pointerdown
        ; mousedown
        ; keydown
        ; transitionend] in
      _listeners <- listeners;

  end
