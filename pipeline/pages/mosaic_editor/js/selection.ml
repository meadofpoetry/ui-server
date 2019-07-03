(* Inspired by https://github.com/Simonwep/selection *)

open Js_of_ocaml
open Js_of_ocaml_lwt
open Components

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.bind

type elt =
  | Query of string
  | Node of Dom_html.element Js.t

type mode =
  | Touch
  | Center
  | Cover

let select_all (elts : elt list) =
  List.fold_left (fun acc -> function
      | Query q ->
        let nodes = Dom_html.document##querySelectorAll (Js.string q) in
        Dom.list_of_nodeList nodes @ acc
      | Node e -> e :: acc) [] elts

let coerce e = (e :> Dom_html.event Js.t)

let parse_event (e : Dom_html.event Js.t) : Dom_html.element Js.t * int * int =
  let tap =
    if Js.Optdef.test (Js.Unsafe.coerce e)##.touches
    && Js.Optdef.test (Js.array_get (Js.Unsafe.coerce e)##.touches 0)
    then
      Js.Optdef.get (Js.array_get (Js.Unsafe.coerce e)##.touches 0)
        (fun () -> assert false)
    else e in
  Dom_html.eventTarget tap,
  (Js.Unsafe.coerce tap)##.clientX,
  (Js.Unsafe.coerce tap)##.clientY

let event_path (e : Dom_html.event Js.t) =
  let path = (Js.Unsafe.coerce e)##.path in
  let path =
    if Js.Optdef.test path
    then path
    else if Js.Optdef.test (Js.Unsafe.coerce e)##.composedPath
    then (Js.Unsafe.coerce e)##composedPath
    else Js.undefined in
  let coerce x = (x :> Dom_html.eventTarget Js.t) in
  Js.Optdef.case path
    (fun () ->
       let target = Dom_html.eventTarget e in
       let rec loop el acc =
         Js.Opt.case (Element.get_parent el)
           (fun () -> acc)
           (fun el -> loop el (coerce el :: acc)) in
       let path = loop target [coerce target] in
       let doc, wnd = Dom_html.(document, window) in
       List.rev (coerce wnd :: coerce doc :: path))
    (Array.to_list % Js.to_array)

let px = Js.string % Printf.sprintf "%gpx"

let get_w_h (rect : Dom_html.clientRect Js.t) =
  Js.Optdef.get rect##.width (fun () -> rect##.right -. rect##.left),
  Js.Optdef.get rect##.height (fun () -> rect##.bottom -. rect##.top)

let collides
    (a : Dom_html.clientRect Js.t)
    (b : Dom_html.clientRect Js.t) = function
  | Cover ->
    b##.left >= a##.left
    && b##.top >= a##.top
    && b##.right <= a##.right
    && b##.bottom <= a##.bottom
  | Touch ->
    a##.right >= b##.left
    && a##.left <= b##.right
    && a##.bottom >= b##.top
    && a##.top <= b##.bottom
  | Center ->
    let bw = Js.Optdef.get b##.width (fun () -> b##.right -. b##.left) in
    let bh = Js.Optdef.get b##.height (fun () -> b##.bottom -. b##.top) in
    let bxc = b##.left +. bw /. 2. in
    let byc = b##.top +. bh /. 2. in
    bxc >= a##.left
    && bxc <= a##.right
    && byc >= a##.top
    && byc <= a##.bottom

type state =
  { mutable area_x1 : float
  ; mutable area_y1 : float
  ; mutable area_x2 : float
  ; mutable area_y2 : float
  ; target_container : Dom_html.element Js.t
  ; target_boundary : Dom_html.clientRect Js.t
  ; scroll_available : bool
  ; mutable scroll_speed : float option * float option
  }

type 'a detail =
  { area : Dom_html.element Js.t
  ; original_event : Dom_html.event Js.t
  ; selected : Dom_html.element Js.t list
  ; removed : Dom_html.element Js.t list
  ; added : Dom_html.element Js.t list
  ; selection : 'a
  }

class t
    ?(multiple = true)
    ?(single_click = true)
    ?(validate_start = fun _ -> true)
    ?(start_threshold = 10)
    ?(scroll_speed_divider = 10.)
    ?(start_areas = [Query "html"])
    ?(boundaries = [Query "html"])
    ?(selectables = [])
    ?(mode = Touch)
    ?(class_ = "selection-area")
    ?(container = Query "body")
    ?on_start
    ?on_select
    ?on_move
    ?on_stop
    (clip : Dom_html.element Js.t)
    () = object(self)

  val doc = Dom_html.document
  val container = List.hd @@ select_all [container]
  val area = Dom_html.(createDiv document)
  val mutable _temp_listeners = []
  val mutable _delayed_listeners = []
  val mutable _listeners = []
  val mutable _selected = []
  val mutable _touched = []
  val mutable _added = []
  val mutable _removed = []
  val mutable _is_single_click = true
  val mutable _single_click = single_click
  val mutable _multiple = multiple
  val mutable _selectables = []
  val mutable _container = None

  inherit Widget.t clip () as super

  method! init () : unit =
    super#init ()

  method! initial_sync_with_dom () : unit =
    area##.style##.top := Js.string "0";
    area##.style##.left := Js.string "0";
    area##.style##.position := Js.string "fixed";

    clip##.style##.overflow := Js.string "hidden";
    clip##.style##.position := Js.string "fixed";
    clip##.style##.transform := Js.string "translate3d(0, 0, 0)";
    clip##.style##.pointerEvents := Js.string "none";
    clip##.style##.zIndex := Js.string "1";
    self#set_disabled false;
    Element.append_child clip area;
    Element.append_child container clip;
    super#initial_sync_with_dom ()

  method! destroy () : unit =
    Element.remove_child_safe clip area;
    Element.remove_child_safe container clip;
    List.iter Lwt.cancel _delayed_listeners;
    List.iter Lwt.cancel _temp_listeners;
    _delayed_listeners <- [];
    _temp_listeners <- [];
    self#set_disabled true;
    super#destroy ()

  method set_single_click (x : bool) : unit =
    _single_click <- x

  method set_multiple (x : bool) : unit =
    _multiple <- x

  method set_disabled (x : bool) : unit =
    if x then self#detach_start_events ()
    else self#attach_start_events ()

  method selected : Dom_html.element Js.t list =
    _selected

  method deselect (elt : Dom_html.element Js.t) : unit =
    _selected <- List.filter (not % Element.equal elt) _selected;
    _touched <- List.filter (not % Element.equal elt) _touched

  method deselect_all () : unit =
    _selected <- []

  method keep_selection () : unit =
    let new' = List.filter (fun x -> not @@ List.memq x _selected) _touched in
    _selected <- _selected @ new'

  method select (items : Dom_html.element Js.t list) : unit =
    let items = List.filter (fun x ->
        (not @@ List.memq x _touched) && (not @@ List.memq x _selected))
        items in
    _added <- items;
    _selected <- _selected @ items

  method select_query (q : string list) : unit =
    self#select @@ select_all (List.map (fun x -> Query x) q)

  (* Private methods *)

  method private notify_event typ (e : Dom_html.event Js.t) : unit =
    let detail =
      { selection = self
      ; original_event = e
      ; area
      ; selected = _touched @ _selected
      ; removed = _removed
      ; added = _added
      } in
    let f = match typ with
      | `Start -> on_start
      | `Move -> on_move
      | `Stop -> on_stop
      | `Selected x ->
        match on_select with
        | None -> None
        | Some f -> Some (f x) in
    match f with
    | None -> ()
    | Some f -> f detail

  method private handle_drag_start (e : Dom_html.event Js.t) _ : unit Lwt.t =
    if not @@ validate_start e
    then Lwt.return_unit
    else
      let target, x, y = parse_event e in
      let rect = target##getBoundingClientRect in
      (* Find start-areas and boundaries *)
      let start_areas = select_all start_areas in
      let boundaries = select_all boundaries in
      (* Check if area starts in one of the start area / boundaries *)
      let evt_path = event_path e in
      let contains_start_areas =
        List.find_opt (fun x ->
            List.exists (fun e -> e == (x :> Dom_html.eventTarget Js.t)) evt_path)
          start_areas in
      let contains_boundaries =
        List.find_opt (fun x ->
            List.exists (fun e -> e == (x :> Dom_html.eventTarget Js.t)) evt_path)
          boundaries in
      (* Check in which container the use currently acts *)
      let target_container = List.find_opt (fun x ->
          collides x##getBoundingClientRect rect mode)
          boundaries in
      match target_container, contains_start_areas, contains_boundaries with
      | None, _, _ | _, None, _ | _, _, None -> Lwt.return_unit
      | Some target_container, _, _ ->
        self#resolve_selectables ();
        let target_boundary = target_container##getBoundingClientRect in
        let width, height = get_w_h target_boundary in
        let scroll_available =
          (float_of_int target_container##.scrollHeight <> Js.math##round height)
          && (float_of_int target_container##.scrollWidth <> Js.math##round width) in
        let state =
          { area_x1 = float_of_int x
          ; area_y1 = float_of_int y
          ; area_x2 = 0.
          ; area_y2 = 0.
          ; target_container
          ; target_boundary
          ; scroll_available
          ; scroll_speed = None, None
          } in
        _touched <- [];
        _added <- [];
        _removed <- [];
        _is_single_click <- true;
        _temp_listeners <- Events.(
            [ seq_loop (make_event @@ Dom_html.Event.make "selectstart")
                doc self#handle_select_start
            ; mouseups doc (self#handle_drag_end state % coerce)
            ; touchcancels doc (self#handle_drag_end state % coerce)
            ; touchends doc (self#handle_drag_end state % coerce)
            ]);
        _delayed_listeners <- Events.(
            [ mousemoves doc (self#handle_delayed_drag_move state % coerce)
            ; touchmoves doc (self#handle_delayed_drag_move state % coerce)
            ]);
        if scroll_available
        then (
          _temp_listeners <- Events.(
              wheels ~passive:false Dom_html.window (self#handle_mouse_wheel state)
              :: _temp_listeners);
          (* The selection-area will also cover other element which are
             out of the current scrollable parent. So find all elements
             which are in the current scrollable element. Later these are
             the only selectables instead of all. *)
          _selectables <- List.filter (fun x ->
              Element.contains target_container x) _selectables;
          (* To clip the area, the selection area has a parent
             which has exact the same dimensions as the scrollable elemeent.
             Now if the area exeeds these boundaries it will be cropped. *)
          clip##.style##.top := px target_boundary##.top;
          clip##.style##.left := px target_boundary##.left;
          clip##.style##.width := px width;
          clip##.style##.height := px height;
          (* The area element is relative to the clipping element,
             but when this is moved or transformed we need to correct
             the positions via a negative margin. *)
          area##.style##.marginTop := px (Float.neg target_boundary##.top);
          area##.style##.marginLeft := px (Float.neg target_boundary##.left))
        else (
          clip##.style##.top := Js.string "0";
          clip##.style##.left := Js.string "0";
          clip##.style##.width := Js.string "100%";
          clip##.style##.height := Js.string "100%";
          area##.style##.marginTop := Js.string "0";
          area##.style##.marginLeft := Js.string "0");
        (* Add class to the area element *)
        Element.add_class area class_;
        Dom.preventDefault e;
        Lwt.return_unit

  method handle_select_start (e : Dom_html.event Js.t) _ : unit Lwt.t =
    Dom.preventDefault e;
    Lwt.return_unit

  method handle_delayed_drag_move (state : state)
      (e : Dom_html.event Js.t) _ : unit Lwt.t =
    let _, x, y = parse_event e in
    (* Check pixel threshold *)
    if Float.abs ((float_of_int (x + y)) -. (state.area_x1 +. state.area_y1))
       >= float_of_int start_threshold
    then (
      List.iter Lwt.cancel _delayed_listeners;
      _delayed_listeners <- [];
      (* An action is recognized as single-select until
         the user performed a multi-selection *)
      _is_single_click <- false;
      if _multiple
      then (
        _temp_listeners <- Events.(
            [ mousemoves doc (self#handle_drag_move state % coerce)
            ; touchmoves ~passive:false doc (self#handle_drag_move state % coerce)
            ] @ _temp_listeners);
        area##.style##.display := Js.string "block";
        self#handle_drag_move state e Lwt.return_unit
        >>= fun () ->
        self#notify_event `Start e;
        Lwt.return_unit)
      else Lwt.return_unit)
    else Lwt.return_unit

  method handle_drag_move (state : state) (e : Dom_html.event Js.t) _ : unit Lwt.t =
    let _, x, y = parse_event e in
    state.area_x2 <- float_of_int x;
    state.area_y2 <- float_of_int y;
    match state.scroll_available, state.scroll_speed with
    | true, (Some _, None | None, Some _) ->
      let rec scroll _ =
        match state.scroll_speed with
        | None, None -> Lwt.return_unit (* Scrolling is not anymore required *)
        | ss_x, ss_y ->
          let scon = state.target_container in
          (* If the value exceeds the scrollable area it will
             be set to the max / min value. So change only *)
          let scroll_top, scroll_left =
            float_of_int state.target_container##.scrollTop,
            float_of_int state.target_container##.scrollLeft in
          (* Reduce velocity, use ceil in both directions to scroll
             at least 1px per frame *)
          (match ss_y with
           | None -> ()
           | Some ss_y ->
             let v = Float.(to_int @@ ceil (ss_y /. scroll_speed_divider)) in
             scon##.scrollTop := scon##.scrollTop + v;
             state.area_y1 <- state.area_y1
                              -. float_of_int scon##.scrollTop
                              -. scroll_top);
          (match ss_x with
           | None -> ()
           | Some ss_x ->
             let v = Float.(to_int @@ ceil (ss_x /. scroll_speed_divider)) in
             scon##.scrollLeft := scon##.scrollLeft + v;
             state.area_x1 <- state.area_x1
                              -. float_of_int scon##.scrollLeft
                              -. scroll_left);
          (* We changed the start coordinates -> redraw the selection area.
             We changed the dimensions of the area element -> re-calc selected elements.
             The selected elements array has been changed -> fire event *)
          self#redraw_area state;
          self#update_touching_elements ();
          self#notify_event `Move e;
          Lwt_js_events.request_animation_frame () >>= scroll in
      Lwt_js_events.request_animation_frame () >>= scroll
    | _ ->
      (* Perform redraw only if scrolling is not active.
         If scrolling is active this area is getting re-drawed by the
         anonymized scroll function *)
      self#redraw_area state;
      self#update_touching_elements ();
      self#notify_event `Move e;
      Lwt.return_unit

  method handle_mouse_wheel (state : state) e _ : unit Lwt.t =
    let ss_x, ss_y =
      let dx = scroll_speed_divider +. float_of_int e##.deltaX in
      let dy = scroll_speed_divider +. float_of_int e##.deltaY in
      (match fst state.scroll_speed with
       | None -> dx
       | Some x -> x +. dx),
      (match snd state.scroll_speed with
       | None -> dy
       | Some y -> y +. dy) in
    state.scroll_speed <- Some ss_x, Some ss_y;
    (* Prevent default scrolling behaviour, e.g. page scrolling *)
    Dom.preventDefault e;
    self#handle_drag_move state (coerce e) Lwt.return_unit

  method handle_single_click (state : state) (e : Dom_html.event Js.t) : unit Lwt.t =
    let rec loop el =
      if List.memq el _selectables
      then Some el
      else Js.Opt.case (Element.get_parent el) (fun () -> None) loop in
    match loop (Dom_html.eventTarget e) with
    | None -> Lwt.return_unit
    | Some target ->
      if false (* FIXME *)
      then (
      )
      else (
        _touched <- [target];
        self#notify_event (`Selected target) e);
      Lwt.return_unit

  method handle_drag_end (state : state) (e : Dom_html.event Js.t) _ : unit Lwt.t =
    List.iter Lwt.cancel _delayed_listeners;
    List.iter Lwt.cancel _temp_listeners;
    _delayed_listeners <- [];
    _temp_listeners <- [];
    if _is_single_click && _single_click
    then (
      self#handle_single_click state e)
    else (
      self#update_touching_elements ();
      self#notify_event `Stop e;
      area##.style##.display := Js.string "none";
      Lwt.return_unit)

  method private update_touching_elements () : unit =
    let rect = area##getBoundingClientRect in
    (* Iterate over selectable elements *)
    let added, touched = List.fold_left (fun ((added, touched) as acc) item ->
        if collides rect item##getBoundingClientRect mode
        then (
          let added =
            if not @@ List.memq item _touched
            then item :: added else added in
          added, item :: touched)
        else acc) ([], []) _selectables in
    (* Check which elements were removed since last selection *)
    let removed = List.filter (fun item ->
        not @@ List.memq item touched) _touched in

    (* Save state *)
    _touched <- touched;
    _removed <- removed;
    _added <- added

  method private attach_start_events () : unit =
    match _listeners with
    | _ :: _ -> () (* Already attached *)
    | [] ->
      _listeners <- Events.(
          [ mousedowns doc (self#handle_drag_start % coerce)
          ; touchstarts ~passive:false doc (self#handle_drag_start % coerce)
          ])

  method private detach_start_events () : unit =
    List.iter Lwt.cancel _listeners;
    _listeners <- []

  method private redraw_area (state : state) : unit =
    let target = state.target_container in
    let rect = state.target_boundary in
    let scroll_left, scroll_top,
        scroll_width, scroll_height,
        client_width, client_height =
      target##.scrollLeft,
      target##.scrollTop,
      target##.scrollWidth,
      target##.scrollHeight,
      target##.clientWidth,
      target##.clientHeight in
    let left, top, width, height =
      rect##.left,
      rect##.top,
      Js.Optdef.get rect##.width (fun () -> rect##.right -. rect##.left),
      Js.Optdef.get rect##.height (fun () -> rect##.bottom -. rect##.top) in
    let x, y = state.area_x2, state.area_y2 in
    let ss_x, x =
      if x < left
      then
        let ss =
          if scroll_left <> 0
          then Some Float.(neg (abs (left -. x)))
          else None in
        ss, left
      else if x > left +. width
      then
        let ss =
          if (scroll_width - scroll_left - client_width) <> 0
          then Some Float.(abs (top +. height -. y))
          else None in
        ss, left +. width
      else None, x in
    let ss_y, y =
      if y < top
      then
        let ss =
          if scroll_top <> 0
          then Some Float.(neg (abs top -. y))
          else None in
        ss, top
      else if y > top +. height
      then
        let ss =
          if (scroll_height - scroll_top - client_height) <> 0
          then Some Float.(abs (top +. height -. y))
          else None in
        ss, top +. height
      else None, y in

    state.scroll_speed <- ss_x, ss_y;

    let x3 = min state.area_x1 x in
    let y3 = min state.area_y1 y in
    let x4 = max state.area_x1 x in
    let y4 = max state.area_y1 y in
    (* Apply styles to area element *)
    area##.style##.top := px y3;
    area##.style##.left := px x3;
    area##.style##.width := px (x4 -. x3);
    area##.style##.height := px (y4 -. y3)

  method private resolve_selectables () : unit =
    _selectables <- select_all selectables

end

type event = t detail

let make
    ?single_click ?validate_start ?start_threshold ?scroll_speed_divider
    ?start_areas ?boundaries ?selectables ?mode ?class_ ?container
    ?on_start ?on_select ?on_move ?on_stop () =
  let elt = Dom_html.(createDiv document) in
  new t ?single_click ?validate_start
    ?start_threshold ?scroll_speed_divider
    ?start_areas ?boundaries ?selectables
    ?mode ?class_ ?container
    ?on_start ?on_select ?on_move ?on_stop
    elt ()
