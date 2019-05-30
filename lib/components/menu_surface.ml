open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Utils

let ( >>= ) = Lwt.bind

include Components_tyxml.Menu_surface
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let transform_prop_name = ref None

let get_transform_property_name ?(force = false) () : string =
  match !transform_prop_name, force with
  | None, _ | _, true ->
    let elt = Dom_html.(createDiv document) in
    let _ = elt##.style in
    if Js.Optdef.test (Js.Unsafe.coerce elt##.style)##.transform
    then "transform" else "webkitTransform"
  | Some v, _ -> v

let get_window_dimensions () : int * int =
  Js.Optdef.get Dom_html.window##.innerWidth (fun () -> 0),
  Js.Optdef.get Dom_html.window##.innerHeight (fun () -> 0)

let get_window_scroll () : int * int =
  (Js.Unsafe.coerce Dom_html.window)##.pageXOffset,
  (Js.Unsafe.coerce Dom_html.window)##.pageYOffset

let get_body_dimensions () : int * int =
  Dom_html.document##.body##.clientWidth,
  Dom_html.document##.body##.clientHeight

module Const = struct
  let transition_open_duration_s = 0.120
  let transition_close_duration_s = 0.75
  let margin_to_edge = 32.
  let anchor_to_menu_surface_width_ratio = 0.67
end

module Event = struct
  let opened : unit Widget.custom_event Js.t Events.Typ.typ =
    Events.Typ.make "menu-surface:opened"
  let closed : unit Widget.custom_event Js.t Events.Typ.typ =
    Events.Typ.make "menu-surface:closed"
end

module Selector = struct
  let focusables =
    "button:not(:disabled), \
     [href]:not([aria-disabled=\"true\"]), \
     input:not(:disabled), \
     select:not(:disabled), \
     textarea:not(:disabled), \
     [tabindex]:not([tabindex=\"-1\"]):not([aria-disabled=\"true\"])"
end

type position =
  { top : float
  ; right : float
  ; left : float
  ; bottom : float
  }

let get_position_value_by_name (pos : position) = function
  | "top" -> pos.top
  | "right" -> pos.right
  | "left" -> pos.left
  | "bottom" -> pos.bottom
  | _ -> invalid_arg "bad position key"

let make_position ?(top = 0.) ? (right = 0.) ?(left = 0.) ?(bottom = 0.) ()
  : position =
  { top; right; left; bottom }

type point =
  { x : float
  ; y : float
  }

type layout =
  { viewport : int * int
  ; viewport_distance : position
  ; anchor_height : float
  ; anchor_width : float
  ; surface_height : float
  ; surface_width : float
  ; body_dimensions : int * int
  ; window_scroll : int * int
  }

module Corner = struct
  type t =
    | Top_left
    | Top_right
    | Bottom_left
    | Bottom_right
    | Top_start
    | Top_end
    | Bottom_start
    | Bottom_end

  let is_bottom = function
    | Bottom_left | Bottom_right | Bottom_start | Bottom_end -> true
    | _ -> false

  let is_right = function
    | Top_right | Bottom_right | Top_end | Bottom_end -> true
    | _ -> false

  let is_flip_rtl = function
    | Top_start | Top_end | Bottom_start | Bottom_end -> true
    | _ -> false

end

let check_finite (x : float) : bool =
  match Float.classify_float x with
  | Float.FP_infinite | Float.FP_nan -> false
  | _ -> true

let is_focused (x : #Dom_html.element Js.t) : bool =
  match Js.Opt.to_option Dom_html.document##.activeElement with
  | None -> false
  | Some active -> Element.equal active x

class t (elt : Dom_html.element Js.t) () =
  object(self)
    inherit Widget.t elt () as super

    val mutable _anchor_element : Dom_html.element Js.t option = None
    val mutable _anchor_corner : Corner.t = Corner.Top_start
    val mutable _anchor_margin : position = make_position ()
    val mutable _quick_open = false
    val mutable _is_open = false
    val mutable _previous_focus = None
    val mutable _hoisted_element = false
    val mutable _is_fixed_position = false

    val mutable _first_focusable = None
    val mutable _last_focusable = None
    val mutable _animation_thread = None

    val mutable _dimensions = 0, 0
    val mutable _position = { x = 0.; y = 0. }
    (* Event listeners. *)
    val mutable _keydown_listener = None
    val mutable _body_click_listener = None

    method! init () : unit =
      super#init ();
      if not @@ super#has_class CSS.root
      then failwith @@ Printf.sprintf "%s class required in root element" CSS.root

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      let parent = match Js.Opt.to_option @@ Element.get_parent super#root with
        | None -> None
        | Some p ->
          if Element.has_class p CSS.anchor
          then Some p else None in
      _anchor_element <- parent;
      if super#has_class CSS.open_ then _is_open <- true;
      if super#has_class CSS.fixed then self#set_fixed_position true;
      (* Attach event listeners. *)
      let keydown = Events.keydowns super#root self#handle_keydown in
      _keydown_listener <- Some keydown

    method! destroy () : unit =
      super#destroy ();
      (* Clear state. *)
      Option.iter Lwt.cancel _animation_thread;
      _animation_thread <- None;
      (* Detach event listeners. *)
      Option.iter Lwt.cancel _keydown_listener;
      _keydown_listener <- None

    method close () : unit Lwt.t =
      if _is_open then self#close_ () else Lwt.return ()

    method reveal () : unit Lwt.t =
      if not _is_open then self#open_ () else Lwt.return ()

    method is_open : bool =
      _is_open

    method set_quick_open (x : bool) : unit =
      _quick_open <- x

    method set_is_hoisted (x : bool) : unit =
      _hoisted_element <- x

    method hoist_menu_to_body () : unit =
      Option.iter (fun parent -> Dom.removeChild parent super#root)
        (Js.Opt.to_option super#root##.parentNode);
      Element.append_child Dom_html.document##.body super#root;
      self#set_is_hoisted true

    method set_anchor_element : 'a. (#Dom_html.element as 'a) Js.t -> unit =
      fun elt -> _anchor_element <- Some (Element.coerce elt)

    method set_fixed_position (x : bool) : unit =
      super#toggle_class ~force:x CSS.fixed;
      _is_fixed_position <- x

    method set_absolute_position (point : point) : unit =
      _position <- { x = if check_finite point.x then point.x else 0.
                   ; y = if check_finite point.y then point.y else 0.
                   };
      self#set_is_hoisted true

    method set_anchor_margin (x : position) : unit =
      _anchor_margin <- x

    method set_anchor_corner (c : Corner.t) : unit =
      _anchor_corner <- c

    (* Private methods *)

    method private handle_open () : unit =
      match _body_click_listener with
      | Some _ -> ()
      | None ->
        let rec listener () =
          Events.click Dom_html.document##.body
          >>= fun e ->
          let target = Dom_html.eventTarget e in
          if not (Element.contains super#root target)
          then self#close_ ()
          else listener () in
        _body_click_listener <- Some (listener ())

    method private handle_close () : unit =
      Option.iter Lwt.cancel _body_click_listener;
      _body_click_listener <- None

    method private open_ () : unit Lwt.t =
      let focusables = Element.query_selector_all super#root Selector.focusables in
      let first_focusable' = List.hd_opt focusables in
      let last_focusable' = List.hd_opt @@ List.rev focusables in
      _first_focusable <- first_focusable';
      _last_focusable <- last_focusable';
      let active = Js.Opt.to_option Dom_html.document##.activeElement in
      _previous_focus <- active;
      if not _quick_open then super#add_class CSS.animating_open;
      let t =
        Animation.request ()
        >>= fun _ ->
        super#add_class CSS.open_;
        _dimensions <- super#root##.offsetWidth, super#root##.offsetHeight;
        self#auto_position ();
        if _quick_open
        then (
          self#notify_open ();
          self#handle_open ();
          Lwt.return ())
        else
          Lwt_js.sleep Const.transition_open_duration_s
          >>= fun () ->
          super#remove_class CSS.animating_open;
          self#notify_open ();
          self#handle_open ();
          Lwt.return () in
      _animation_thread <- Some t;
      Lwt.on_termination t (fun () -> _animation_thread <- None);
      _is_open <- true;
      t

    method private close_ () : unit Lwt.t =
      if not _quick_open then super#add_class CSS.animating_closed;
      let t =
        Animation.request ()
        >>= fun _ ->
        super#remove_class CSS.open_;
        if _quick_open
        then (
          self#notify_close ();
          self#handle_close ();
          Lwt.return ())
        else
          Lwt_js.sleep Const.transition_close_duration_s
          >>= fun () ->
          super#remove_class CSS.animating_closed;
          self#notify_close ();
          self#handle_close ();
          Lwt.return () in
      _animation_thread <- Some t;
      Lwt.on_termination t (fun () ->
          _animation_thread <- None;
          self#maybe_restore_focus ());
      _is_open <- false;
      t

    method private handle_keydown (e : Dom_html.keyboardEvent Js.t)
        (_ : unit Lwt.t) : unit Lwt.t =
      let shift = Js.to_bool e##.shiftKey in
      match Events.Key.of_event e with
      | `Escape -> self#close ()
      | `Tab ->
        let check_focused = function
          | None -> false
          | Some x -> is_focused x in
        if check_focused _last_focusable && not shift
        then (Option.iter (fun x -> x##focus) _first_focusable;
              Dom.preventDefault e;
              Lwt.return_unit)
        else if check_focused _first_focusable && shift
        then (Option.iter (fun x -> x##focus) _last_focusable;
              Dom.preventDefault e;
              Lwt.return_unit)
        else Lwt.return_unit
      | _ -> Lwt.return ()

    method private get_origin_corner ({ viewport_distance = dist
                                      ; anchor_height
                                      ; anchor_width
                                      ; surface_height
                                      ; surface_width
                                      ; _
                                      } : layout) : Corner.t =
      let is_bottom_aligned = Corner.is_bottom _anchor_corner in
      let available_top =
        if is_bottom_aligned
        then dist.top +. anchor_height +. _anchor_margin.bottom
        else dist.top +. _anchor_margin.top in
      let available_bot =
        if is_bottom_aligned
        then dist.bottom -. _anchor_margin.bottom
        else dist.bottom +. anchor_height -. _anchor_margin.top in
      let top_overflow = surface_height -. available_top in
      let bot_overflow = surface_height -. available_bot in
      let is_rtl = super#is_rtl () in
      let is_flip_rtl = Corner.is_flip_rtl _anchor_corner in
      let avoid_hor_overlap = Corner.is_right _anchor_corner in
      let is_aligned_right =
        (avoid_hor_overlap && not is_rtl)
        || (not avoid_hor_overlap && is_flip_rtl && is_rtl) in
      let available_left =
        if is_aligned_right
        then dist.left +. anchor_width +. _anchor_margin.right
        else dist.left +. _anchor_margin.left in
      let available_right =
        if is_aligned_right
        then dist.right -. _anchor_margin.right
        else dist.right +. anchor_width -. _anchor_margin.left in
      let left_overflow = surface_width -. available_left in
      let right_overflow = surface_width -. available_right in
      let is_bottom = bot_overflow >. 0. && top_overflow <. bot_overflow in
      let is_right =
        (left_overflow <. 0. && is_aligned_right && is_rtl)
        || (avoid_hor_overlap && not is_aligned_right && left_overflow <. 0.)
        || (right_overflow >. 0. && left_overflow <. right_overflow) in
      match is_bottom, is_right with
      | false, false -> Top_left
      | false, true -> Top_right
      | true, false -> Bottom_left
      | true, true -> Bottom_right

    method private maybe_restore_focus () : unit =
      Js.Opt.iter Dom_html.document##.activeElement (fun active ->
          Option.iter (fun prev ->
              if Element.contains super#root active then prev##focus)
            _previous_focus)

    method private notify_open () : unit =
      super#emit Event.opened

    method private notify_close () : unit =
      super#emit Event.closed

    method private get_horizontal_origin_offset
        ({ anchor_width; viewport; body_dimensions; _ } : layout)
        (corner : Corner.t) : float =
      let avoid_horizontal_overlap = Corner.is_right _anchor_corner in
      if Corner.is_right corner
      then (
        let right_offset =
          if avoid_horizontal_overlap
          then anchor_width -. _anchor_margin.left
          else _anchor_margin.right in
        (* For hoisted or fixed elements, adjust the offset by the difference
             between viewport width and body width so when we calculate the right
             value (`adjustPositionForHoistedElement_`) based on the element
             position, the right property is correct.
        *)
        if _hoisted_element || _is_fixed_position
        then let diff = fst viewport - fst body_dimensions in
          right_offset -. float_of_int diff
        else right_offset)
      else if avoid_horizontal_overlap
      then anchor_width -. _anchor_margin.right
      else _anchor_margin.left

    method private get_vertical_origin_offset
        ({ anchor_height; _ } : layout)
        (corner : Corner.t) : float =
      let avoid_vertical_overlap = Corner.is_bottom _anchor_corner in
      if Corner.is_bottom corner
      then
        if avoid_vertical_overlap
        then anchor_height -. _anchor_margin.top
        else _anchor_margin.bottom
      else if avoid_vertical_overlap
      then anchor_height +. _anchor_margin.bottom
      else _anchor_margin.top

    method private get_menu_surface_max_height
        ({ viewport_distance = dist; anchor_height; _ } : layout)
        (corner : Corner.t) : float =
      if Corner.is_bottom corner
      then
        let h = dist.top +. _anchor_margin.top -. Const.margin_to_edge in
        if Corner.is_bottom _anchor_corner
        then h else h +. anchor_height
      else
        let h = dist.bottom -. _anchor_margin.bottom -. Const.margin_to_edge in
        if Corner.is_bottom _anchor_corner
        then h else h +. anchor_height

    method private auto_position () : unit =
      (* Compute measurements for autoposition methods reuse. *)
      let meas = self#get_auto_layout_measurements () in
      let corner = self#get_origin_corner meas in
      let valign = if Corner.is_bottom corner then "bottom" else "top" in
      let halign = if Corner.is_right corner then "right" else "left" in
      let voffset = self#get_vertical_origin_offset meas corner in
      let hoffset = self#get_horizontal_origin_offset meas corner in
      let position = [(valign, voffset); (halign, hoffset)] in
      let { anchor_width; surface_width; _ } = meas in
      (* Center align when anchor width is comparable or greater than
         menu surface, otherwise keep corner. *)
      let halign =
        if anchor_width /. surface_width >. Const.anchor_to_menu_surface_width_ratio
        then "center" else halign in
      let position =
        (* If the menu-surface has been hoisted to the body, it's no longer
           relative to the anchor element. *)
        if _hoisted_element || _is_fixed_position
        then self#adjust_position_for_hoisted_element meas position
        else position in
      Js.Unsafe.set super#root##.style
        (Js.string (get_transform_property_name () ^ "-origin"))
        (Js.string (Printf.sprintf "%s %s" halign valign));
      self#set_position position;
      begin match self#get_menu_surface_max_height meas corner with
        | 0. -> super#root##.style##.maxHeight := Js.string ""
        | x -> super#root##.style##.maxHeight := px_js (int_of_float x)
      end

    method private adjust_position_for_hoisted_element
        ({ window_scroll = x, y
         ; viewport_distance = dist
         ; _ } : layout)
        (position : (string * float) list)
      : (string * float) list =
      List.map (fun (k, v) ->
          (* Hoisted surfaces need to have the anchor elements location
             on the page added to the position properties for proper alignment
             on the body. *)
          let v = v +. get_position_value_by_name dist k in
          let v =
            (* Surfaces that are absolutely positioned need to have
               additional calculations for scroll and bottom positioning. *)
            if _is_fixed_position then v else
              match k with
              | "top" | "bottom" -> v +. float_of_int y
              | "left" | "right" -> v +. float_of_int x
              | _ -> v in
          k, v) position

    method private get_auto_layout_measurements () : layout =
      let anchor_rect = Option.map (fun e -> e##getBoundingClientRect) _anchor_element in
      let viewport = get_window_dimensions () in
      let body = get_body_dimensions () in
      let scroll = get_window_scroll () in
      let anchor_rect = match anchor_rect with
        | Some x -> x
        | None ->
          object%js
            val top = _position.y
            val bottom = _position.y
            val left = _position.x
            val right = _position.x
            val height = Js.def 0.
            val width = Js.def 0.
          end in
      { viewport
      ; viewport_distance =
          { top = anchor_rect##.top
          ; right = (float_of_int @@ fst viewport) -. anchor_rect##.right
          ; left = anchor_rect##.left
          ; bottom = (float_of_int @@ snd viewport) -. anchor_rect##.bottom
          }
      ; body_dimensions = body
      ; window_scroll = scroll
      ; anchor_height = Js.Optdef.get anchor_rect##.height (fun () -> 0.)
      ; anchor_width = Js.Optdef.get anchor_rect##.width (fun () -> 0.)
      ; surface_height = float_of_int @@ snd _dimensions
      ; surface_width = float_of_int @@ fst _dimensions
      }

    method private set_position (pos : (string * float) list) : unit =
      let get = List.Assoc.get ~eq:String.equal in
      let conv = function
        | None -> Js.string ""
        | Some s -> Js.string @@ Printf.sprintf "%gpx" s in
      super#root##.style##.top := conv (get "top" pos);
      super#root##.style##.bottom := conv (get "bottom" pos);
      super#root##.style##.right := conv (get "right" pos);
      super#root##.style##.left := conv (get "left" pos)
  end

let make ?fixed ?open_ (content : Dom_html.element Js.t list) : t =
  let content' = List.map Tyxml_js.Of_dom.of_element content in
  let (elt : Dom_html.divElement Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?fixed ?open_ content' () in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
