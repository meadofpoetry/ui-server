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
  List.fold_left
    (fun acc -> function
      | Query q ->
          let nodes = Dom_html.document##querySelectorAll (Js.string q) in
          Dom.list_of_nodeList nodes @ acc
      | Node e -> e :: acc)
    []
    elts

let coerce e = (e :> Dom_html.event Js.t)

let parse_event (e : Dom_html.event Js.t) : Dom_html.element Js.t * float * float =
  let tap =
    if Js.Optdef.test (Js.Unsafe.coerce e)##.touches
       && Js.Optdef.test (Js.array_get (Js.Unsafe.coerce e)##.touches 0)
    then
      Js.Optdef.get
        (Js.array_get (Js.Unsafe.coerce e)##.touches 0)
        (fun () -> assert false)
    else e
  in
  ( Dom_html.eventTarget tap
  , float_of_int (Js.Unsafe.coerce tap)##.clientX
  , float_of_int (Js.Unsafe.coerce tap)##.clientY )

let event_path (e : Dom_html.event Js.t) =
  let path = (Js.Unsafe.coerce e)##.path in
  let path =
    if Js.Optdef.test path
    then path
    else if Js.Optdef.test (Js.Unsafe.coerce e)##.composedPath
    then (Js.Unsafe.coerce e)##composedPath
    else Js.undefined
  in
  let coerce x = (x :> Dom_html.eventTarget Js.t) in
  Js.Optdef.case
    path
    (fun () ->
      let target = Dom_html.eventTarget e in
      let rec loop el acc =
        Js.Opt.case
          (Element.get_parent el)
          (fun () -> acc)
          (fun el -> loop el (coerce el :: acc))
      in
      let path = loop target [coerce target] in
      let doc, wnd = Dom_html.(document, window) in
      List.rev (coerce wnd :: coerce doc :: path))
    (Array.to_list % Js.to_array)

let px = Js.string % Printf.sprintf "%gpx"

let get_w_h (rect : Dom_html.clientRect Js.t) =
  ( Js.Optdef.get rect##.width (fun () -> rect##.right -. rect##.left)
  , Js.Optdef.get rect##.height (fun () -> rect##.bottom -. rect##.top) )

let collides (a : Dom_html.clientRect Js.t) (b : Dom_html.clientRect Js.t) = function
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
      let bxc = b##.left +. (bw /. 2.) in
      let byc = b##.top +. (bh /. 2.) in
      bxc >= a##.left && bxc <= a##.right && byc >= a##.top && byc <= a##.bottom

type state =
  { mutable ax1 : float
  ; mutable ay1 : float
  ; mutable ax2 : float
  ; mutable ay2 : float
  ; target_container : Dom_html.element Js.t
  ; mutable target_boundary : Dom_html.clientRect Js.t
  ; mutable scroll_available : bool
  ; mutable scroll_speed : float option * float option }

type event =
  { area : Dom_html.element Js.t
  ; original_event : Dom_html.event Js.t
  ; selected : Dom_html.element Js.t list
  ; removed : Dom_html.element Js.t list
  ; added : Dom_html.element Js.t list }

class t
  ?(multiple = true)
  ?(single_click = true)
  ?(start_threshold = 10)
  ?(scroll_speed_divider = 10.)
  ?(start_areas = [Query "html"])
  ?(boundaries = [Query "html"])
  ?(selectables = [])
  ?(mode = Touch)
  ?(class_ = "selection-area")
  ?(container = Query "body")
  ?before_start
  ?on_start
  ?on_move
  ?on_stop
  ?on_outside_click
  (clip : Dom_html.element Js.t)
  () =
  object (self)
    val doc = Dom_html.document

    val container = List.hd @@ select_all [container]

    val area = Dom_html.(createDiv document)

    val mutable _temp_listeners = []

    val mutable _delayed_listeners = []

    val mutable _listeners = []

    val mutable _selected = []

    val mutable _stored = []

    val mutable _added = []

    val mutable _removed = []

    val mutable _is_single_click = true

    val mutable _single_click = single_click

    val mutable _multiple = multiple

    val mutable _mode = mode

    val mutable _selectables = []

    inherit Widget.t clip () as super

    method! init () : unit =
      Element.append_child clip area;
      (* Add class to the area element *)
      Element.add_class area class_;
      (* Apply basic styles to the area element *)
      let will_change = Js.string "top, left, bottom, right, width, height" in
      (Js.Unsafe.coerce area##.style)##.willChange := will_change;
      area##.style##.top := Js.string "0";
      area##.style##.left := Js.string "0";
      area##.style##.position := Js.string "fixed";
      (* Add styles to the clipping element *)
      clip##.style##.overflow := Js.string "hidden";
      clip##.style##.position := Js.string "fixed";
      clip##.style##.transform := Js.string "translate3d(0, 0, 0)";
      clip##.style##.pointerEvents := Js.string "none";
      clip##.style##.zIndex := Js.string "101";
      (* FIXME *)
      self#set_disabled false;
      super#init ()

    method! destroy () : unit =
      Element.remove_child_safe clip area;
      Element.remove_child_safe container clip;
      List.iter Lwt.cancel _delayed_listeners;
      List.iter Lwt.cancel _temp_listeners;
      _delayed_listeners <- [];
      _temp_listeners <- [];
      self#set_disabled true;
      super#destroy ()

    method set_mode mode : unit = _mode <- mode

    method set_single_click (x : bool) : unit = _single_click <- x

    method set_multiple (x : bool) : unit = _multiple <- x

    method set_disabled (x : bool) : unit =
      if x then self#detach_start_events () else self#attach_start_events ()

    method selected : Dom_html.element Js.t list = _stored

    method remove_from_selection (elt : Dom_html.element Js.t) : unit =
      _removed <- elt :: _removed;
      _stored <- List.filter (not % Element.equal elt) _stored;
      _selected <- List.filter (not % Element.equal elt) _selected

    method clear_selection ?(store = true) () : unit =
      if store then _stored <- [];
      _selected <- [];
      _added <- [];
      _removed <- []

    method resolve_selectables () : unit = _selectables <- select_all selectables

    method keep_selection () : unit =
      _stored <-
        List.fold_left
          (fun acc x -> if not @@ List.memq x acc then x :: acc else acc)
          _stored
          _selected

    method select (items : Dom_html.element Js.t list) : unit =
      let items =
        List.filter
          (fun x -> (not @@ List.memq x _selected) && (not @@ List.memq x _stored))
          items
      in
      _selected <- _selected @ items;
      _added <- _added @ items

    method select_query (q : string list) : unit =
      self#select @@ select_all (List.map (fun x -> Query x) q)

    (* Private methods *)
    method private make_detail e =
      {original_event = e; area; selected = _selected; removed = _removed; added = _added}

    method private notify_event_bool typ (e : Dom_html.event Js.t) : bool =
      let f =
        match typ with
        | `Before_start -> before_start
      in
      match f with
      | None -> true
      | Some f -> f (self :> t) (self#make_detail e)

    method private notify_event typ (e : Dom_html.event Js.t) : unit =
      let f =
        match typ with
        | `Start -> on_start
        | `Move -> on_move
        | `Stop -> on_stop
        | `Outside_click -> on_outside_click
      in
      match f with
      | None -> ()
      | Some f -> f (self :> t) (self#make_detail e)

    method private handle_drag_start (e : Dom_html.event Js.t) _ : unit Lwt.t =
      let target, x, y = parse_event e in
      let rect = target##getBoundingClientRect in
      (* Find start-areas and boundaries *)
      let start_areas = select_all start_areas in
      let boundaries = select_all boundaries in
      (* Check in which container the use currently acts *)
      let target_container =
        List.find_opt (fun x -> collides x##getBoundingClientRect rect _mode) boundaries
      in
      (* Check if area starts in one of the start area / boundaries *)
      let evt_path = event_path e in
      let contains_start_areas =
        List.exists
          (fun x -> List.exists (( == ) (x :> Dom_html.eventTarget Js.t)) evt_path)
          start_areas
      in
      let contains_boundaries =
        List.exists
          (fun x -> List.exists (( == ) (x :> Dom_html.eventTarget Js.t)) evt_path)
          boundaries
      in
      match target_container, contains_start_areas, contains_boundaries with
      | None, _, _ | _, false, _ | _, _, false -> Lwt.return_unit
      | Some target_container, true, true ->
          if self#notify_event_bool `Before_start e
          then (
            let state =
              (* Area start point *)
              { ax1 = x
              ; ay1 = y (* Area end point *)
              ; ax2 = 0.
              ; ay2 = 0.
              ; target_container
              ; target_boundary =
                  object%js
                    val left = 0.

                    val right = 0.

                    val top = 0.

                    val bottom = 0.

                    val width = Js.undefined

                    val height = Js.undefined
                  end
              ; scroll_available = false
              ; scroll_speed = None, None }
            in
            (* To detect single click *)
            _is_single_click <- true;
            _selected <- [];
            self#clear_selection ~store:false ();
            _temp_listeners <-
              Lwt_js_events.
                [ (* Prevent default select event *)
                  seq_loop
                    (make_event @@ Dom_html.Event.make "selectstart")
                    doc
                    self#handle_select_start
                ; mouseups doc (self#handle_drag_end state % coerce)
                ; dragends doc (self#handle_drag_end ~no_event:true state % coerce)
                ; touchcancels doc (self#handle_drag_end state % coerce)
                ; touchends doc (self#handle_drag_end state % coerce) ];
            _delayed_listeners <-
              Lwt_js_events.
                [ mousemoves doc (self#handle_delayed_drag_move state % coerce)
                ; touchmoves doc (self#handle_delayed_drag_move state % coerce) ];
            Lwt.return_unit)
          else Lwt.return_unit

    method private handle_select_start (e : Dom_html.event Js.t) _ : unit Lwt.t =
      Dom.preventDefault e;
      Lwt.return_unit

    method private handle_single_click (_state : state) (e : Dom_html.event Js.t) : unit
        =
      let target = Dom.eventTarget e in
      (* Resolve selectables again.
       If the user starded in a scrollable area they will be reduced
       to the current area. Prevent the exclusion of these if a range-selection
       gets performed. *)
      self#resolve_selectables ();
      let rec includes el =
        if List.memq el _selectables
        then Some el
        else Js.Opt.case (Element.get_parent el) (fun () -> None) includes
      in
      (* Traverse dom upwards to check if target is selectable *)
      match includes target with
      | None -> self#notify_event `Outside_click e
      | Some target -> (
          self#notify_event `Start e;
          let shift_key =
            Js.Opt.case
              (Dom_html.CoerceTo.mouseEvent e)
              (fun () -> false)
              (fun x -> Js.to_bool x##.shiftKey)
          in
          match shift_key, _stored with
          | true, _ :: _ -> () (* TODO *)
          | _ ->
              if List.memq target _stored
              then self#remove_from_selection target
              else self#select [target];
              self#notify_event `Move e;
              self#notify_event `Stop e)

    method private init_multiple_selection e ({target_container; _} as state) =
      _temp_listeners <-
        Lwt_js_events.(
          [ mousemoves doc (self#handle_drag_move state % coerce)
          ; touchmoves ~passive:false doc (self#handle_drag_move state % coerce) ]
          @ _temp_listeners);
      (* Make area element visible. *)
      area##.style##.display := Js.string "block";
      (* Append selection-area to the dom *)
      Element.append_child container clip;
      (* Now after the threshold is reached resolve all selectables. *)
      self#resolve_selectables ();
      let target_boundary = target_container##getBoundingClientRect in
      state.target_boundary <- target_boundary;
      let top, left = target_boundary##.top, target_boundary##.left in
      let width, height = get_w_h target_boundary in
      let round x = int_of_float @@ Float.round x in
      let scroll_available =
        target_container##.scrollHeight <> round height
        && target_container##.scrollWidth <> round width
      in
      state.scroll_available <- scroll_available;
      if scroll_available
      then (
        (* Detect mouse scrolling *)
        _temp_listeners <-
          Lwt_js_events.(
            seq_loop
              (make_event @@ Dom_html.Event.make "wheel")
              ~passive:false
              Dom_html.window
              (self#manual_scroll state)
            :: _temp_listeners);
        (* The selection-area will also cover other element which are
         out of the current scrollable parent. So find all elements
         which are in the current scrollable element. Later these are
         the only selectables instead of all. *)
        _selectables <- List.filter (Element.contains target_container) _selectables;
        (* To clip the area, the selection area has a parent
         which has exact the same dimensions as the scrollable elemeent.
         Now if the area exeeds these boundaries it will be cropped. *)
        clip##.style##.top := px top;
        clip##.style##.left := px left;
        clip##.style##.width := px width;
        clip##.style##.height := px height;
        (* The area element is relative to the clipping element,
         but when this is moved or transformed we need to correct
         the positions via a negative margin. *)
        area##.style##.marginTop := px (Float.neg top);
        area##.style##.marginLeft := px (Float.neg left))
      else (
        (* Reset margin and clipping element dimensions *)
        clip##.style##.top := Js.string "0";
        clip##.style##.left := Js.string "0";
        clip##.style##.width := Js.string "100%";
        clip##.style##.height := Js.string "100%";
        area##.style##.marginTop := Js.string "0";
        area##.style##.marginLeft := Js.string "0");
      (* Trigger recalc and fire event *)
      self#handle_drag_move state e Lwt.return_unit
      >>= fun () ->
      self#notify_event `Start e;
      Lwt.return_unit

    method private handle_delayed_drag_move
        ({ax1; ay1; _} as state : state)
        (e : Dom_html.event Js.t)
        _
        : unit Lwt.t =
      let _, x, y = parse_event e in
      (* Check pixel threshold *)
      (if Float.abs (x +. y -. (ax1 +. ay1)) >= float_of_int start_threshold
      then (
        List.iter Lwt.cancel _delayed_listeners;
        _delayed_listeners <- [];
        (* An action is recognized as single-select until
          the user performed a multi-selection *)
        _is_single_click <- false;
        if _multiple then self#init_multiple_selection e state else Lwt.return_unit)
      else Lwt.return_unit)
      >>= fun () ->
      Dom.preventDefault e;
      Lwt.return_unit

    method private handle_drag_move
        (state : state)
        (e : Dom_html.event Js.t)
        _
        : unit Lwt.t =
      let _, x, y = parse_event e in
      state.ax2 <- x;
      state.ay2 <- y;
      (* Prevent swipe-down refresh *)
      Dom.preventDefault e;
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
                  float_of_int scon##.scrollTop, float_of_int scon##.scrollLeft
                in
                (* Reduce velocity, use ceil in both directions to scroll
             at least 1px per frame *)
                (match ss_y with
                | None -> ()
                | Some ss_y ->
                    let v = Float.(to_int @@ ceil (ss_y /. scroll_speed_divider)) in
                    scon##.scrollTop := scon##.scrollTop + v;
                    state.ay1 <- state.ay1 -. float_of_int scon##.scrollTop -. scroll_top);
                (match ss_x with
                | None -> ()
                | Some ss_x ->
                    let v = Float.(to_int @@ ceil (ss_x /. scroll_speed_divider)) in
                    scon##.scrollLeft := scon##.scrollLeft + v;
                    state.ax1 <-
                      state.ax1 -. float_of_int scon##.scrollLeft -. scroll_left);
                (* We changed the start coordinates -> redraw the selection area.
             We changed the dimensions of the area element -> re-calc selected elements.
             The selected elements array has been changed -> fire event *)
                self#redraw_area state;
                self#update_touching_elements ();
                self#notify_event `Move e;
                (* Keep scrolling even if the user stops to move his pointer *)
                Lwt_js_events.request_animation_frame () >>= scroll
          in
          Lwt_js_events.request_animation_frame () >>= scroll
      | _ ->
          (* Perform redraw only if scrolling is not active.
         If scrolling is active this area is getting re-drawed by the
         anonymized scroll function *)
          self#redraw_area state;
          self#update_touching_elements ();
          self#notify_event `Move e;
          Lwt.return_unit

    method private handle_drag_end
        ?(no_event = false)
        (state : state)
        (e : Dom_html.event Js.t)
        _
        : unit Lwt.t =
      List.iter Lwt.cancel _delayed_listeners;
      List.iter Lwt.cancel _temp_listeners;
      _delayed_listeners <- [];
      _temp_listeners <- [];
      if _is_single_click && _single_click
      then self#handle_single_click state e
      else if (not _is_single_click) && not no_event
      then (
        self#update_touching_elements ();
        self#notify_event `Stop e);
      (* Remove selection-area from dom *)
      let () = (Js.Unsafe.coerce clip)##remove in
      area##.style##.display := Js.string "none";
      Lwt.return_unit

    method private update_touching_elements () : unit =
      let area_rect = area##getBoundingClientRect in
      (* Iterate over selectable elements *)
      let added, touched =
        List.fold_left
          (fun ((added, touched) as acc) item ->
            if collides area_rect item##getBoundingClientRect _mode
            then
              let added =
                (* Check if the element wasn't in the last selection *)
                if not @@ List.memq item _selected then item :: added else added
              in
              added, item :: touched
            else acc)
          ([], [])
          _selectables
      in
      (* Check which elements were removed since last selection *)
      let removed = List.filter (not % Fun.flip List.memq touched) _selected in
      (* Save *)
      _selected <- touched;
      _removed <- removed;
      _added <- added

    method private attach_start_events () : unit =
      match _listeners with
      | _ :: _ -> () (* Already attached *)
      | [] ->
          _listeners <-
            Lwt_js_events.
              [ mousedowns doc (self#handle_drag_start % coerce)
              ; touchstarts ~passive:false doc (self#handle_drag_start % coerce) ]

    method private detach_start_events () : unit =
      List.iter Lwt.cancel _listeners;
      _listeners <- []

    method private manual_scroll state e _ =
      let add x = function
        | None -> x
        | Some o -> o +. x
      in
      state.scroll_speed <-
        (let dy = scroll_speed_divider *. (float_of_int e##.deltaY *. -1.) in
         let dx = scroll_speed_divider +. (float_of_int e##.deltaX *. -1.) in
         Some (add dx (fst state.scroll_speed)), Some (add dy (snd state.scroll_speed)));
      (* Prevent default scrolling behaviour, e.g. page scrolling *)
      Dom.preventDefault e;
      self#handle_drag_move state (coerce e) Lwt.return_unit

    method private redraw_area (state : state) : unit =
      let target = state.target_container in
      let rect = state.target_boundary in
      let ( scroll_left
          , scroll_top
          , scroll_width
          , scroll_height
          , client_width
          , client_height ) =
        ( target##.scrollLeft
        , target##.scrollTop
        , target##.scrollWidth
        , target##.scrollHeight
        , target##.clientWidth
        , target##.clientHeight )
      in
      let left, top, width, height =
        ( rect##.left
        , rect##.top
        , Js.Optdef.get rect##.width (fun () -> rect##.right -. rect##.left)
        , Js.Optdef.get rect##.height (fun () -> rect##.bottom -. rect##.top) )
      in
      let x, y = state.ax2, state.ay2 in
      let ss_x, x =
        if x < left
        then
          let ss =
            if scroll_left <> 0 then Some Float.(neg (abs (left -. x))) else None
          in
          ss, left
        else if x > left +. width
        then
          let ss =
            if scroll_width - scroll_left - client_width <> 0
            then Some Float.(abs (top +. height -. y))
            else None
          in
          ss, left +. width
        else None, x
      in
      let ss_y, y =
        if y < top
        then
          let ss = if scroll_top <> 0 then Some Float.(neg (abs top -. y)) else None in
          ss, top
        else if y > top +. height
        then
          let ss =
            if scroll_height - scroll_top - client_height <> 0
            then Some Float.(abs (top +. height -. y))
            else None
          in
          ss, top +. height
        else None, y
      in
      state.scroll_speed <- ss_x, ss_y;
      let x3 = min state.ax1 x in
      let y3 = min state.ay1 y in
      let x4 = max state.ax1 x in
      let y4 = max state.ay1 y in
      (* Apply styles to area element *)
      area##.style##.top := px y3;
      area##.style##.left := px x3;
      area##.style##.width := px (x4 -. x3);
      area##.style##.height := px (y4 -. y3)
  end

let make
    ?multiple
    ?single_click
    ?start_threshold
    ?scroll_speed_divider
    ?start_areas
    ?boundaries
    ?selectables
    ?mode
    ?class_
    ?container
    ?before_start
    ?on_start
    ?on_move
    ?on_stop
    ?on_outside_click
    () =
  let elt = Dom_html.(createDiv document) in
  new t
    ?multiple
    ?single_click
    ?before_start
    ?start_threshold
    ?scroll_speed_divider
    ?start_areas
    ?boundaries
    ?selectables
    ?mode
    ?class_
    ?container
    ?on_start
    ?on_move
    ?on_stop
    ?on_outside_click
    elt
    ()
