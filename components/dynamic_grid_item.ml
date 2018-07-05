open Containers
open Dynamic_grid_types
open Dom_events

include Dynamic_grid_cell

type action = Mouse of Dom_html.mouseEvent Js.t
            | Touch of Dom_html.touchEvent Js.t

let to_item ?min_w ?min_h ?max_w ?max_h ?(keep_ar=false)
            ?(resizable=true) ?(draggable=true) ?(selectable=true)
            ?on_resize ?on_resizing ?on_drag ?on_dragging
            ?close_widget ?move_widget ?widget ~pos ~value () =
  { pos; min_w; min_h; max_w; max_h; keep_ar; resizable; draggable; selectable;
    on_resize; on_resizing; on_drag; on_dragging;
    close_widget; move_widget; widget; value}


let listen ?save target typ f =
  let listener = Dom_events.listen target typ f in
  Option.iter (fun f -> f @@ Some listener) save

let stop_listen x = Option.iter Dom_events.stop_listen x

let rec find_touch id num source =  (* given a touchList Js.t, finds a touch with needed identifier among *)
  if num < 0 then None
  else
    if Js.Optdef.test (source##item num)
    then let touch = Js.Optdef.get (source##item num) (fun () -> failwith "No touch with such id") in
         if touch##.identifier = id
         then Some touch
         else find_touch id (num - 1) source
    else find_touch id (num - 1) source

let eq x y = Equal.physical x#root y#root

let filter ~(exclude:#Widget.t list) (l:#Widget.t list) =
  List.filter (fun x -> not (List.mem ~eq x exclude)) l

class ['a] t ~s_grid        (* grid props *)
           ~(item: 'a item) (* item props *)
           ~e_modify_push   (* add/delete item event *)
           ~s_selected      (* selected items *)
           ~s_selected_push (* selected items signal modifier *)
           ~s_col_w         (* column width signal -- px *)
           ~s_row_h         (* row height signal   -- px *)
           ~(s_items : 'a t list React.signal) (* items signal *)
           () =
  let s_value,s_value_push = React.S.create item.value in
  object(self)

    inherit ['a] cell ~typ:`Item ~s_col_w ~s_row_h ~s_grid ~pos:item.pos () as super

    (* FIXME make normal signal *)
    val s_change      = React.S.create ~eq:(fun _ _ -> false) item.pos
    val ghost         = new cell ~typ:`Ghost ~s_col_w ~s_row_h ~s_grid ~pos:item.pos ()
    val resize_button = Markup.Item.create_resize_button ()
                        |> Tyxml_js.To_dom.of_element |> Widget.create

    val _s_draggable_global  = React.S.map ~eq:(Equal.option Equal.bool) (fun x -> x.draggable) s_grid
    val _s_resizable_global  = React.S.map ~eq:(Equal.option Equal.bool) (fun x -> x.resizable) s_grid
    val _s_selectable_global = React.S.map ~eq:(Equal.option Equal.bool) (fun x -> x.selectable) s_grid

    val mutable _main_move_mouse_listener   = None
    val mutable _main_move_touch_listener   = None
    val mutable _main_resize_mouse_listener = None
    val mutable _main_resize_touch_listener = None
    val mutable _main_select_listener       = None

    val mutable mov_listener    = None
    val mutable end_listener    = None
    val mutable cancel_listener = None

    val mutable draggable       = item.draggable
    val mutable resizable       = item.resizable
    val mutable selectable      = item.selectable

    val mutable _draggable_global  = None
    val mutable _resizable_global  = None
    val mutable _selectable_global = None

    val mutable selected        = false
    val mutable drag_timer      = None

    val mutable keep_ar = item.keep_ar
    val mutable min_w   = item.min_w
    val mutable min_h   = item.min_h
    val mutable max_w   = item.max_w
    val mutable max_h   = item.max_h

    (** API **)

    method inner_widget = item.widget

    method set_pos (p:Position.t)   = super#set_pos p; ghost#set_pos p
    method set_min_w (x:int option) = min_w <- x
    method set_min_h (x:int option) = min_h <- x
    method set_max_w (x:int option) = max_w <- x
    method set_max_h (x:int option) = max_h <- x
    method set_keep_ar (x:bool)     = keep_ar <- x

    method s_changing = ghost#s_pos
    (* FIXME this is ok, but will raise change event even before dragging is not over *)
    method s_change   = self#s_pos
    method s_value    = s_value

    method value : 'a       = React.S.value self#s_value
    method set_value (x:'a) = s_value_push x

    method draggable        = draggable
    method set_draggable x  = self#_set_draggable x; draggable <- x

    method resizable        = resizable
    method set_resizable x  = self#_set_resizable x; resizable <- x

    method selectable       = selectable
    method set_selectable x = self#_set_selectable x; selectable <- x

    method remove () : unit = self#set_selected false; e_modify_push (`Remove self)

    method selected              = selected
    method set_selected x : unit =
      let o = React.S.value s_selected in
      match x with
      | true  -> if self#grid.multi_select
                 then (if not self#selected then s_selected_push ((self :> 'a t) :: o))
                 else (List.iter (fun x -> if not (eq x self) then x#set_selected false) o;
                       s_selected_push [(self :> 'a t)]);
                 self#add_class Markup.Item.selected_class;
                 selected <- true
      | false -> if self#selected
                 then (self#remove_class Markup.Item.selected_class;
                       selected <- false;
                       s_selected_push @@ List.filter (fun x -> not @@ eq x self) o)

    method layout () =
      Option.iter (fun x -> x#layout ()) self#inner_widget;
      Option.iter (fun x -> x#layout ()) item.move_widget;
      Option.iter (fun x -> x#layout ()) item.close_widget

    (** Private methods **)

    method private _set_draggable x =
      (match x with
       | true  -> self#add_move_listener ()
       | false -> self#stop_move_listener ());
      self#get_drag_target#add_or_remove_class x Markup.Item.drag_handle_class

    method private _set_resizable x =
      (match x with
       | true  -> self#add_resize_listener ()
       | false -> self#stop_resize_listener ());
      if x then Dom.appendChild self#root resize_button#root
      else (try Dom.removeChild self#root resize_button#root; with _ -> ())

    method private _set_selectable x =
      (match x with
       | true  -> self#add_select_listener (); self#set_attribute "tabindex" "0"
       | false -> self#stop_select_listener ();
                  self#set_attribute "tabindex" "-1";
                  self#set_selected false);
      self#add_or_remove_class x Markup.Item.select_handle_class

    method private grid = React.S.value s_grid

    method private get_drag_target = match item.move_widget with
      | Some w -> w
      | None   -> (self :> Widget.t)

    method private items = React.S.value s_items

    method private has_collision (pos : Position.t) =
      Position.has_collision ~f:(fun x -> x#pos) pos (List.filter Fun.(eq self %> not) self#items)

    method private get_parent : Dom_html.element Js.t =
      Js.Opt.to_option self#root##.parentNode |> Option.map Js.Unsafe.coerce |> Option.get_exn

    method private mouse_action meth ev =
      let init_pos = px_pos in
      let init_x, init_y = ev##.clientX, ev##.clientY in
      listen ~save:(fun x -> mov_listener <- x) Dom_html.window Typ.mousemove (fun _ ev ->
               let x,y = ev##.clientX, ev##.clientY in
               meth ~x ~y ~init_x ~init_y ~init_pos `Move;
               false);
      listen ~save:(fun x -> end_listener <- x) Dom_html.window Typ.mouseup (fun _ ev ->
               if ev##.button = 0
               then (let x, y = ev##.clientX, ev##.clientY in
                     meth ~x ~y ~init_x ~init_y ~init_pos `End);
               false);

    method private touch_action meth ev =
      let init_pos = px_pos in
      Js.Optdef.iter
        (ev##.touches##item (ev##.touches##.length - 1))
        (fun touch ->
          let id = touch##.identifier in
          let init_x, init_y = touch##.clientX, touch##.clientY in
          listen ~save:(fun x -> mov_listener <- x) Dom_html.window Typ.touchmove (fun _ ev ->
                   (match find_touch id (ev##.changedTouches##.length-1) ev##.changedTouches with
                    | Some touch -> meth ~x:touch##.clientX ~y:touch##.clientY ~init_x ~init_y ~init_pos `Move
                    | None       -> ());
                   false);
          listen ~save:(fun x -> end_listener <- x) Dom_html.window Typ.touchend (fun _ ev ->
                   (match find_touch id (ev##.changedTouches##.length-1) ev##.changedTouches with
                    | Some touch -> meth ~x:touch##.clientX ~y:touch##.clientY ~init_x ~init_y ~init_pos `End
                    | None       -> ());
                   false);
          listen ~save:(fun x -> cancel_listener <- x) Dom_html.window Typ.touchcancel (fun _ ev ->
                   (match find_touch id (ev##.changedTouches##.length-1) ev##.changedTouches with
                    | Some touch -> meth ~x:touch##.clientX ~y:touch##.clientY ~init_x ~init_y ~init_pos `End
                    | None -> ());
                   false))

    method private resolve_pos_conflicts ~action (pos : Position.t) =
      let other     = filter ~exclude:[(self :> 'a t)] self#items in
      let f         = (fun x -> x#pos) in
      let cols,rows = self#grid.cols, self#grid.rows in
      let new_pos = match List.filter (fun x -> Position.collides pos x#pos) other with
        | [] -> pos
        | l  -> (match self#grid.vertical_compact with
                 | false -> ghost#pos
                 | true  ->
                    let bind f    = function [] -> f () | l -> l in
                    let (>>=) x f = bind f x in
                    let check_top () = match action with
                      | `Size -> []
                      | `Drag -> Position.move_top ~f ~eq ~collisions:l pos other in
                    let check_bot ()  = Position.move_down ?rows ~f ~eq ~collisions:l pos other in
                    let check_swap () = Position.swap ~cols ~f ~eq ~collisions:l ~ghost_pos:ghost#pos pos other in
                    let res = check_top () >>= check_bot >>= check_swap >>= (fun () -> []) in
                    match res with
                    | [] -> ghost#pos
                    | l  -> List.iter (fun (pos,item) -> item#set_pos pos) l; pos)
      in
      (match action with
       | `Drag -> if self#grid.vertical_compact
                  then ghost#set_pos @@ Position.compact ~f new_pos other
                  else ghost#set_pos new_pos;
       | `Size -> ghost#set_pos new_pos);
      if self#grid.vertical_compact;
      then List.iter (fun x -> let lst = filter ~exclude:[(x:>'a t)] other |> List.map f |> List.cons ghost#pos in
                               let pos = Position.compact ~f:(fun x -> x) x#pos lst in
                               x#set_pos pos)
                     (Position.sort_by_y ~f other)

    method private start_dragging (ev: action) =
      stop_listen mov_listener;
      stop_listen end_listener;
      stop_listen cancel_listener;
      if self#draggable
      then
        (self#get_drag_target#add_class Markup.Item.dragging_class;
         ghost#style##.zIndex := Js.string "1";
         self#style##.zIndex  := Js.string "3";
         (* add ghost item to dom to show possible element position *)
         Dom.appendChild self#get_parent ghost#root;
         match ev with
         | Mouse ev -> self#mouse_action self#apply_position ev
         | Touch ev -> self#touch_action self#apply_position ev)

    method private apply_position ~x ~y ~init_x ~init_y ~init_pos typ =
      match x, y with
      | 0,0 -> ()
      | _   ->
         let col_px, row_px = React.S.value s_col_w, React.S.value s_row_h in
         match typ with
         | `Move->
            let open Utils in
            let cols,rows = self#grid.cols,self#grid.rows in
            let x,y = init_pos.x + x - init_x, init_pos.y + y - init_y in
            let pos = Position.correct_xy { self#pos with x = x // col_px
                                                        ; y = y // row_px }
                                          cols rows in
            self#resolve_pos_conflicts ~action:`Drag pos;
            let x,y = if self#grid.restrict_move
                      then (let pos = Position.correct_xy { self#px_pos with x;y }
                                                          (cols * col_px)
                                                          (Option.map (fun x -> x * row_px) rows)
                            in
                            pos.x, pos.y)
                      else x,y
            in
            self#set_x x; self#set_y y;
            self#layout ();
            Option.iter (fun f -> f self#pos ghost#pos col_px row_px) item.on_dragging
         | `End->
            self#get_drag_target#remove_class Markup.Item.dragging_class;
            Option.iter (fun l -> Dom_events.stop_listen l) mov_listener;
            Option.iter (fun l -> Dom_events.stop_listen l) end_listener;
            Option.iter (fun l -> Dom_events.stop_listen l) cancel_listener;
            self#style##.zIndex := Js.string "";
            (* update element position from ghost *)
            self#set_x @@ React.S.value s_col_w * ghost#pos.x;
            self#set_y @@ React.S.value s_row_h * ghost#pos.y;
            self#set_pos ghost#pos;
            Dom.removeChild self#get_parent ghost#root;
            if self#grid.vertical_compact
            then List.iter (fun x -> let lst = filter ~exclude:[(x:>'a t)] self#items in
                                     let pos = Position.compact ~f:(fun x -> x#pos) x#pos lst in
                                     x#set_pos pos)
                           (Position.sort_by_y ~f:(fun x -> x#pos) self#items);
            (snd s_change) self#pos;
            self#layout ();
            Option.iter (fun f -> f self#pos ghost#pos col_px row_px) item.on_drag
         | _ -> ()

    method private start_resizing (ev: action) =
      if self#resizable
      then (ghost#style##.zIndex := Js.string "4";
            self#style##.zIndex  := Js.string "3";
            Dom.appendChild self#get_parent ghost#root;
            (* add resize/stop resize event listeners *)
            match ev with
            | Mouse ev -> Dom_html.stopPropagation ev; self#mouse_action self#apply_size ev
            | Touch ev -> Dom_html.stopPropagation ev; self#touch_action self#apply_size ev)

    method private apply_size ~x ~y ~init_x ~init_y ~init_pos typ =
      let col_px, row_px = React.S.value s_col_w, React.S.value s_row_h in
      match typ with
      | `Move ->
         let open Utils in
         let cols,rows = self#grid.cols, self#grid.rows in
         let w,h = init_pos.w + x - init_x, init_pos.h + y - init_y in
         let pos = Position.correct_wh ?max_w ?min_w ?max_h ?min_h
                                       { self#pos with w = w // col_px
                                                     ; h = h // row_px }
                                       cols rows
         in
         let pos  = if not keep_ar then pos
                    else let resolution = self#pos.w,self#pos.h in
                         let aspect     = Utils.resolution_to_aspect resolution in
                         Position.correct_aspect pos aspect
         in
         self#resolve_pos_conflicts ~action:`Size pos;
         let w,h = if self#grid.restrict_move
                   then (let pos = Position.correct_wh { self#px_pos with w;h }
                                                       (cols * col_px)
                                                       (Option.map (fun x -> x * row_px) rows)
                         in
                         pos.w, pos.h)
                   else w,h
         in
         self#set_w w; self#set_h h;
         self#layout ();
         Option.iter (fun f -> f self#pos ghost#pos col_px row_px) item.on_resizing
      | `End ->
         stop_listen mov_listener;
         stop_listen end_listener;
         stop_listen cancel_listener;
         self#style##.zIndex := Js.string "";
         (* update element position from ghost *)
         self#set_w @@ React.S.value s_col_w * ghost#pos.w;
         self#set_h @@ React.S.value s_row_h * ghost#pos.h;
         self#set_pos ghost#pos;
         Dom.removeChild self#get_parent ghost#root;
         if self#grid.vertical_compact
         then List.iter (fun x -> let lst = filter ~exclude:[(x:>'a t)] self#items in
                                  let pos = Position.compact ~f:(fun x -> x#pos) x#pos lst in
                                  x#set_pos pos)
                        (Position.sort_by_y ~f:(fun x -> x#pos) self#items);
         (snd s_change) self#pos;
         self#layout ();
         Option.iter (fun f -> f self#pos ghost#pos col_px row_px) item.on_resize
      | _ -> ()

    method private stop_move_listener () = stop_listen _main_move_mouse_listener;
                                           stop_listen _main_move_touch_listener;
                                           _main_move_mouse_listener <- None;
                                           _main_move_touch_listener <- None
    method private add_move_listener () =
      self#stop_move_listener ();
      listen ~save:(fun l -> _main_move_mouse_listener <- l)
             self#get_drag_target#root Typ.mousedown
             (fun _ e -> Dom_html.stopPropagation e;
                         listen ~save:(fun x -> mov_listener <- x) Dom_html.window Typ.mousemove
                                (fun _ _ -> if e##.button = 0 && draggable
                                            then self#start_dragging (Mouse e);
                                            false);
                         listen ~save:(fun x -> end_listener <- x) Dom_html.window Typ.mouseup
                                (fun _ _ -> stop_listen mov_listener; false);
                         true);
      listen ~save:(fun l -> _main_move_touch_listener <- l)
             self#get_drag_target#root Typ.touchstart
             (fun _ e -> Dom_html.stopPropagation e;
                         if self#selectable && not self#grid.multi_select then self#set_selected true;
                         if e##.touches##.length <= 1
                         then
                           ( let timer   = 400. in                 (**  TIMER is here **)
                             let touch   = Js.Optdef.get (e##.touches##item 0)
                                                         (fun () -> failwith "No touch with such id") in
                             let id      = touch##.identifier in
                             let wrapped = Js.wrap_callback
                                             (fun _ -> if draggable
                                                       then self#start_dragging (Touch e))
                             in
                             let timeout = Dom_html.window##setTimeout wrapped timer in
                             let stop_timeout e =
                               let touch = Js.Optdef.get (e##.changedTouches##item 0)
                                                         (fun () -> failwith "No touch with such id") in
                               if touch##.identifier = id
                               then (Dom_html.window##clearTimeout timeout;
                                     stop_listen mov_listener;
                                     stop_listen end_listener;
                                     stop_listen cancel_listener)
                             in
                             listen ~save:(fun x -> mov_listener <- x) Dom_html.window Typ.touchmove
                                    (fun _ ev ->
                                      (match find_touch id (ev##.changedTouches##.length-1) ev##.changedTouches with
                                       | Some touch1 ->
                                          let dx = abs @@ touch1##.clientX - touch##.clientX in
                                          let dy = abs @@ touch1##.clientY - touch##.clientY in
                                          if dx > 8 || dy > 8
                                          then (Dom_html.window##clearTimeout timeout;
                                                stop_listen mov_listener;
                                                stop_listen end_listener;
                                                stop_listen cancel_listener);
                                       | None       -> ());
                                      false);
                             listen ~save:(fun x -> cancel_listener <- x) Dom_html.window Typ.touchcancel
                                    (fun _ e -> stop_timeout e; false);
                             listen ~save:(fun x -> end_listener <- x) Dom_html.window Typ.touchend
                                    (fun _ e -> stop_timeout e; false));
                         false);

    method private stop_resize_listener () = stop_listen _main_resize_mouse_listener;
                                             stop_listen _main_resize_touch_listener;
                                             _main_resize_mouse_listener <- None;
                                             _main_resize_touch_listener <- None
    method private add_resize_listener () =
      self#stop_resize_listener ();
      listen ~save:(fun l -> _main_resize_mouse_listener <- l)
             resize_button#root Typ.mousedown (fun _ e ->
               Dom_html.stopPropagation e;
               Dom.preventDefault e;
               if e##.button = 0 && resizable
               then self#start_resizing (Mouse e);
               false);
      listen ~save:(fun l -> _main_resize_touch_listener <- l)
             resize_button#root Typ.touchstart (fun _ e ->
               Dom_html.stopPropagation e;
               if e##.touches##.length <= 1
               then (if resizable then self#start_resizing (Touch e));
               false)

    method private stop_select_listener () = stop_listen _main_select_listener;
                                             _main_select_listener <- None
    method private add_select_listener () =
      self#stop_select_listener ();
      listen ~save:(fun l -> _main_select_listener <- l)
             self#root Typ.focus (fun _ _ ->
               if self#selectable && not self#grid.multi_select then self#set_selected true; true);

    initializer
      (React.S.map (function
                    | Some x -> self#_set_draggable x
                    | None   -> self#_set_draggable self#draggable) _s_draggable_global |> ignore);
      (React.S.map (function
                    | Some x -> self#_set_resizable x
                    | None   -> self#_set_resizable self#resizable) _s_resizable_global |> ignore);
      (React.S.map (function
                    | Some x -> self#_set_selectable x
                    | None   -> self#_set_selectable self#selectable) _s_selectable_global |> ignore);
      (* add close listener to close widget if provided *)
      Option.iter (fun x -> Dom_events.listen x#root Dom_events.Typ.click (fun _ _ ->
                                                self#remove (); true) |> ignore) item.close_widget;
      (* append widget to cell if provided *)
      Option.iter (fun x -> Dom.appendChild self#root x#root) item.widget;

  end
