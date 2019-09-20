open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Tab_scroller
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

(* TODO
   - add RTL support
   - remove 'animating' boolean
 *)

let ( >>= ) = Lwt.bind

type animation =
  { final_scroll_position : int
  ; scroll_delta : int }

let compute_horizontal_scroll_height () : int option =
  let el = Dom_html.(createDiv document) in
  el##.classList##add (Js.string CSS.scroll_test);
  Dom.appendChild Dom_html.document##.body el;
  let height = el##.offsetHeight - el##.clientHeight in
  Dom.removeChild Dom_html.document##.body el;
  Some height

module Selector = struct
  let scroll_content = Printf.sprintf ".%s" CSS.scroll_content

  let scroll_area = Printf.sprintf ".%s" CSS.scroll_area
end

class t (elt : Dom_html.element Js.t) () =
  object (self)
    val scroll_content = Element.query_selector_exn elt Selector.scroll_content

    val scroll_area = Element.query_selector_exn elt Selector.scroll_area

    val mutable hscroll_height = 0

    val mutable animating : bool = false

    val mutable listeners = []

    val mutable tabs =
      List.map Tab.attach
      @@ Element.query_selector_all elt (Printf.sprintf ".%s" Tab.CSS.root)

    inherit Widget.t elt () as super

    method! init () : unit =
      (* Compute horizontal scrollbar height on scroller with overflow initially hidden,
       then update overflow to scroll and immediately adjust bottom margin to avoid
         the scrollbar initially appearing before JS runs *)
      let margin_bottom =
        match compute_horizontal_scroll_height () with
        | Some x -> Js.string (Printf.sprintf "-%dpx" x)
        | None -> Js.string ""
      in
      scroll_area##.style##.marginBottom := margin_bottom;
      Element.add_class scroll_area CSS.scroll_area_scroll;
      super#init ()

    method! initial_sync_with_dom () : unit =
      (* Attach event listeners *)
      let handle_interaction _ _ =
        self#handle_interaction ();
        Lwt.return_unit
      in
      let handle_transitionend e _ =
        self#handle_transition_end e;
        Lwt.return_unit
      in
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ touchstarts ~passive:true super#root handle_interaction
          ; pointerdowns super#root handle_interaction
          ; mousedowns super#root handle_interaction
          ; keydowns super#root handle_interaction
          ; seq_loop
              ~passive:true
              (make_event @@ Dom_html.Event.make "wheel")
              super#root
              handle_interaction
          ; seq_loop
              (make_event @@ Dom_html.Event.make "transitionend")
              super#root
              handle_transitionend ]
          @ listeners);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      (* Detach event listeners *)
      List.iter Lwt.cancel listeners;
      listeners <- [];
      super#destroy ()

    method! layout () : unit =
      List.iter Widget.layout tabs;
      super#layout ()

    method tabs : Tab.t list = tabs

    method remove_tab (tab : Tab.t) : unit =
      tabs <- List.filter (fun x -> not @@ Widget.equal tab x) tabs;
      Element.remove_child_safe super#root tab#root;
      self#layout ()

    method append_tab (tab : Tab.t) : unit =
      tabs <- tab :: tabs;
      Element.append_child super#root tab#root;
      self#layout ()

    method insert_tab_at_index (i : int) (tab : Tab.t) : unit =
      tabs <- tab :: tabs;
      Element.insert_child_at_index super#root i tab#root;
      self#layout ()

    method get_tab_at_index (i : int) : Tab.t option =
      List.find_opt (fun (tab : Tab.t) -> tab#index = i) tabs

    method active_tab : Tab.t option =
      List.find_opt (fun (tab : Tab.t) -> tab#active) tabs

    method set_active_tab (tab : Tab.t) : unit =
      match self#active_tab with
      | None -> tab#set_active true
      | Some previous ->
          tab#set_active ~previous true;
          previous#set_active false

    method align : align option =
      if super#has_class CSS.align_start
      then Some Start
      else if super#has_class CSS.align_end
      then Some End
      else if super#has_class CSS.align_center
      then Some Center
      else None

    method set_align (x : align option) : unit =
      super#remove_class CSS.align_start;
      super#remove_class CSS.align_end;
      super#remove_class CSS.align_center;
      match x with
      | None -> ()
      | Some Start -> super#add_class CSS.align_start
      | Some End -> super#add_class CSS.align_end
      | Some Center -> super#add_class CSS.align_center

    method content_width : int = scroll_content##.offsetWidth

    (* Computes the current visual scroll position *)
    method get_scroll_position () : int =
      let current_translate_x = self#get_current_translate_x () in
      let scroll_left = scroll_area##.scrollLeft in
      scroll_left - current_translate_x

    (* Scrolls to the given scrollX value *)
    method scroll_to (scroll_x : int) : unit Lwt.t =
      let current_scroll_x = self#get_scroll_position () in
      let safe_scroll_x = self#clamp_scroll_value scroll_x in
      let scroll_delta = safe_scroll_x - current_scroll_x in
      self#animate {scroll_delta; final_scroll_position = safe_scroll_x}

    (* Increment scroll value by the given value *)
    method increment_scroll (scroll_x : int) : unit Lwt.t =
      match scroll_x with
      | 0 -> Lwt.return_unit
      | x ->
          let current_scroll_x = self#get_scroll_position () in
          let target_scroll_x = x + current_scroll_x in
          let safe_scroll_x = self#clamp_scroll_value target_scroll_x in
          let scroll_delta = safe_scroll_x - current_scroll_x in
          self#animate {scroll_delta; final_scroll_position = safe_scroll_x}

    (* Handles interaction events that occur during transition *)
    method private handle_interaction () : unit =
      if animating then self#stop_scroll_animation ()

    (* Handles transitionend event *)
    method private handle_transition_end e : unit =
      Js.Opt.iter e##.target (fun (target : Dom_html.element Js.t) ->
          if animating && Element.matches target ("." ^ CSS.scroll_content)
          then (
            animating <- false;
            super#remove_class CSS.animating))

    method private calculate_scroll_edges () : int * int =
      let content_width = scroll_content##.offsetWidth in
      let root_width = scroll_area##.offsetWidth in
      (* left, right *)
      0, content_width - root_width

    (* Calculates a safe scroll value that is > 0 and < the max scroll value
     * v - the distance to scroll
     *)
    method private clamp_scroll_value (v : int) : int =
      let left, right = self#calculate_scroll_edges () in
      min (max left v) right

    method private get_current_translate_x () =
      let style = Dom_html.window##getComputedStyle scroll_content in
      let value = Js.to_string style##.transform in
      match value with
      | "none" -> 0
      | value -> (
        try
          int_of_float
          @@ float_of_string
          @@ String.trim
          @@ List.nth (String.split_on_char ',' value) 4
        with _ -> 0)

    (* Gets the current scroll position during animation *)
    method private get_animating_scroll_position () : int =
      let current_translate_x = self#get_current_translate_x () in
      let scroll_left = scroll_area##.scrollLeft in
      scroll_left - current_translate_x

    (* Stops scroll animation *)
    method private stop_scroll_animation () : unit =
      animating <- false;
      let current_scroll_position = self#get_animating_scroll_position () in
      super#remove_class CSS.animating;
      super#root##.style##.transform := Js.string "translateX(0px)";
      scroll_area##.scrollLeft := current_scroll_position

    (* Animates the tab scrolling *)
    method private animate (a : animation) : unit Lwt.t =
      (* Early exit if translateX is 0, which means
       * there is no animation to perform *)
      if a.scroll_delta = 0
      then Lwt.return_unit
      else (
        self#stop_scroll_animation ();
        scroll_area##.scrollLeft := a.final_scroll_position;
        let translate_x = Printf.sprintf "translateX(%dpx)" a.scroll_delta in
        scroll_content##.style##.transform := Js.string translate_x;
        (* Force repaint *)
        ignore @@ scroll_area##getBoundingClientRect;
        animating <- true;
        Js_of_ocaml_lwt.Lwt_js_events.request_animation_frame ()
        >>= fun () ->
        super#add_class CSS.animating;
        scroll_content##.style##.transform := Js.string "none";
        Lwt.return_unit)
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

let make ?classes ?a ?align ?tabs ?scroll_area () =
  D.tab_scroller ?classes ?a ?align ?tabs ?scroll_area ()
  |> Tyxml_js.To_dom.of_div
  |> attach
