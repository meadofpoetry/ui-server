open Containers

module Utils = struct

  (** rounds a number the right way **)
  let round x =
    if Float.(x < (floor x +. 0.5))
    then int_of_float @@ floor x
    else int_of_float @@ ceil x

  let px = Printf.sprintf "%dpx"

  let translate = Printf.sprintf "translate(%dpx, %dpx)"

  let (//) x y =
    round @@ (float_of_int x) /. (float_of_int y)

end

module Position = struct

  type t =
    { x : int
    ; y : int
    ; w : int
    ; h : int
    }

  let empty = { x = 0; y = 0; w = 0; h = 0 }

  let equal a b = a.x = b.x && a.y = b.y && a.w = b.w && a.h = b.h

  let to_string (pos:t) = Printf.sprintf "x=%d, y=%d, w=%d, h=%d" pos.x pos.y pos.w pos.h

  (* checks if two elements collide, returns true if do and false otherwise *)
  let collides (pos1:t) (pos2:t) =
    if (pos1.x + pos1.w <= pos2.x)      then false
    else if (pos1.x >= pos2.x + pos2.w) then false
    else if (pos1.y + pos1.h <= pos2.y) then false
    else if (pos1.y >= pos2.y + pos2.h) then false
    else true

  (** get first element that collides with position **)
  let get_first_collision ~(f:'a -> t) pos (l:'a list) =
    List.fold_while (fun acc (x:'a) -> if collides pos (f x)
                                       then (Some x,`Stop)
                                       else (acc,`Continue)) None l

  (** get all elements that collides with position **)
  let get_all_collisions ~(f:'a -> t) pos (l:'a list) =
    List.fold_left (fun acc (x:'a) -> if collides pos (f x)
                                      then x::acc else acc) [] l

  (** check if element collides with other elements **)
  let has_collision ~(f:'a -> t) x (l:'a list) =
    Option.is_some @@ get_first_collision ~f x l

  (** compacts the position vertically **)
  let compact ~(f:'a -> t) (pos:t) (l:'a list) : t =
    let rec up (pos:t) =
      let y = pos.y - 1 in
      if y >= 0 && (not @@ has_collision ~f { pos with y } l)
      then up { pos with y }
      else pos in
    up pos

  (** Sorts positions by top **)
  let sort_by_y ~(f:'a -> t) (l:'a list) =
    List.sort (fun p1 p2 -> compare (f p1).y (f p2).y) l

  (** Given a list of collisions and overall items in grid, resolve these collisions by moving down **)
  let rec move_down ?rows ~f ~(eq:'a -> 'a -> bool) ~(collisions:'a list) (pos : t) (l:'a list) =
    let y            = pos.y + pos.h in
   (* let move x acc  =
      let new_pos    = {(f x) with y} in
      let new_list   = (new_pos, x)::acc in
      let filtered   = List.filter (fun i -> eq x i) l in
      let collisions = List.filter (fun x -> collides new_pos (f x)) filtered in
      let further    = move_down ?rows ~f ~eq ~collisions new_pos filtered in
      List.append new_list further
    in*)
    match rows with
    | None      -> List.fold_left (fun acc x ->
                       let new_pos    = {(f x) with y} in
                       let new_list   = (new_pos, x)::acc in
                       let filtered   = List.filter (fun i -> eq x i) l in
                       let collisions = List.filter (fun x -> collides new_pos (f x)) filtered in
                       let further    = move_down ?rows ~f ~eq ~collisions new_pos filtered in
                       List.append new_list further) [] collisions
    | Some rows -> List.fold_while (fun acc x ->
                       if y + (f x).h <= rows
                       then ( let new_pos    = {(f x) with y} in
                              let new_list   = (new_pos, x)::acc in
                              let filtered   = List.filter (fun i -> eq x i) l in
                              let collisions = List.filter (fun x -> collides new_pos (f x)) filtered in
                              let further    = move_down ~rows ~f ~eq ~collisions new_pos filtered in
                              let result     = List.append new_list further in
                              match collisions, further with
                              | _::[], [] -> [], `Stop
                              | _::_, []  -> [], `Stop
                              | _,_       -> result, `Continue)
                       else [], `Stop) [] collisions

  (** Given a list of collisions and overall items in grid, resolve these collisions by moving up **)
  let move_top ~f ~(eq:'a -> 'a -> bool) ~(collisions:'a list) (pos : t) (l:'a list) =
    List.fold_while (fun acc x ->
        let lst = List.filter (fun i -> eq x i) l in
        let new_pos = compact ~f (f x) lst in
        if new_pos.y + new_pos.h <= pos.y
        then ((new_pos, x) :: acc), `Continue
        else [], `Stop) [] collisions

  (** Swap positions if possible **)
  let swap (pos_to_place: t) (pos_to_move : t) =
    if pos_to_place.w <= pos_to_move.w && pos_to_place.h <= pos_to_move.h
    then Some {pos_to_place with x = pos_to_move.x
                               ; y = pos_to_move.y}
    else None

  let get_free_rect ?(cmp:(t -> t -> int) option) ~(f:'a -> t) (pos:t) (items:'a list) w h () =
    if has_collision ~f:(fun x -> x) pos items
    then None
    else
      let area pos = pos.w * pos.h in
      let cmp = match cmp with
        | Some f -> f
        | None   ->
           (fun new_pos old_pos ->
             let new_area = area new_pos in
             let old_area = area old_pos in
             compare new_area old_area)
      in
      let items    = List.map f items in
      (* FIXME obviously not optimized algorithm *)
      (* get only elements that are on the way to cursor proection to the left/right side *)
      let x_filtered = List.filter (fun i -> pos.y > i.y && pos.y < i.y + i.h) items in
      (* get cursor proection to the left side *)
      let l = List.filter (fun i -> i.x < pos.x) x_filtered
              |> List.fold_left (fun acc i -> if i.x + i.w > acc.x + acc.w then i else acc) empty
              |> (fun x -> x.x + x.w) in
      (* get cursor proection to the right side *)
      let r =
        List.filter (fun i -> i.x > pos.x) x_filtered
        |> List.fold_left (fun acc i -> if i.x < acc.x then i else acc)
                          { x=w; y=0; w=0; h=0 }
        |> (fun x -> x.x) in
      (* get only elements that are on the way to cursor proection to the top/bottom side *)
      let y_filtered = List.filter
                         (fun i -> pos.x > i.x && pos.x < i.x + i.w && i.x + i.w > l && i.x < r)
                         items in
      (* get cursor proection to the top side *)
      let t = List.filter (fun i -> i.y < pos.y) y_filtered
              |> List.fold_left (fun acc i -> if i.y + i.h > acc.y + acc.h then i else acc) empty
              |> (fun x -> x.y + x.h) in
      (* get cursor proection to the bottom side *)
      let b =
        List.filter (fun i -> i.y > pos.y) y_filtered
        |> List.fold_left (fun acc i -> if i.y < acc.y then i else acc)
                          { x=0; y=h; w=0; h=0}
        |> (fun x -> x.y) in
      (* get available x points, FIXME obviously we don't need to iterate over all items *)
      let xs = List.fold_left (fun acc i ->
                   let join = fun x lst -> if x >= l && x <= r then x :: lst else lst in
                   let acc  = join i.x acc |> join (i.x + i.w) in
                   acc) [] items
               |> (fun x -> l :: x @ [r])
               |> List.sort_uniq ~cmp:(Pervasives.compare)
      in
      (* get available y points, FIXME obviously we don't need to iterate over all items *)
      let ys = List.fold_left (fun acc i ->
                   let join = fun y lst -> if y >= t && y <= b then y :: lst else lst in
                   let acc  = join i.y acc |> join (i.y + i.h) in
                   acc) [] items
               |> (fun x -> t :: x @ [b])
               |> List.sort_uniq ~cmp:(Pervasives.compare)
      in
      (* get biggest non-overlapping rectangle under the cursor *)
      (* FIXME obviously not optimized at all *)
      let a = List.fold_left (fun acc x0 ->
                  let xs = List.filter (fun i -> i > x0) xs in
                  List.fold_left (fun acc x1 ->
                      List.fold_left (fun acc y0 ->
                          let ys = List.filter (fun i -> i > y0) ys in
                          List.fold_left (fun acc y1 ->
                              let (new_pos:t) = { x = x0; y = y0; w = x1 - x0; h = y1 - y0 } in
                              (*
                               * new rect must be the biggest one available,
                               * it must not overlap with other rects,
                               * it must be under the mouse cursor
                               *)
                              match (cmp new_pos acc),
                                    get_first_collision ~f:(fun x -> x) new_pos items,
                                    collides pos new_pos with
                              | 1, None, true -> new_pos
                              | _             -> acc) acc ys) acc ys) acc xs)
                             empty xs
      in
      if equal a empty then None else Some a

  let correct_xy (p:t) par_w par_h =
    let x = if p.x < 0 then 0 else if p.x + p.w > par_w then par_w - p.w else p.x in
    let y = match par_h with
      | Some ph -> if p.y < 0 then 0 else if p.y + p.h > ph then ph - p.h else p.y
      | None    -> if p.y < 0 then 0 else p.y
    in
    { p with x;y }

  let correct_w ?max_w ?(min_w=1) (p:t) par_w =
    let w = match max_w with
      | Some max -> if p.w > max then max else if p.w < min_w then min_w else p.w
      | None     -> if p.w < min_w then min_w else p.w
    in
    let w = if p.x + w > par_w then par_w - p.x else w in
    { p with w }

  let correct_h ?max_h ?(min_h=1) (p:t) par_h =
    let h = match max_h with
      | Some max -> if p.h > max then max else if p.h < min_h then min_h else p.h
      | None     -> if p.h < min_h then min_h else p.h
    in
    let h = match par_h with
      | Some ph -> if p.y + h > ph then ph - p.y else h
      | None    -> h
    in
    { p with h }

  let correct_wh ?max_w ?min_w ?max_h ?min_h p par_w par_h =
    correct_w ?max_w ?min_w p par_w
    |> (fun p -> correct_h ?max_h ?min_h p par_h)

end

type 'a item =
  { pos         : Position.t
  ; min_w       : int option
  ; min_h       : int option
  ; max_w       : int option
  ; max_h       : int option
  ; resizable   : bool
  ; draggable   : bool
  ; selectable  : bool

  ; move_widget : Widget.widget option
  ; widget      : Widget.widget option
  ; value       : 'a

  ; on_resize   : (Position.t -> Position.t -> int -> int -> unit) option
  ; on_resizing : (Position.t -> Position.t -> int -> int -> unit) option
  ; on_drag     : (Position.t -> Position.t -> int -> int -> unit) option
  ; on_dragging : (Position.t -> Position.t -> int -> int -> unit) option
  }

type grid =
  { min_col_width    : int
  ; max_col_width    : int option
  ; cols             : int
  ; rows             : int option
  ; row_height       : int option
  ; vertical_compact : bool
  ; items_margin     : (int * int) option
  ; multi_select     : bool
  ; restrict_move    : bool
  }

module Item = struct

  type action = Mouse of Dom_html.mouseEvent Js.t
              | Touch of Dom_html.touchEvent Js.t

  let to_item ?min_w ?min_h ?max_w ?max_h
              ?(resizable=true) ?(draggable=true) ?(selectable=false)
              ?on_resize ?on_resizing ?on_drag ?on_dragging
              ?move_widget ?widget ~pos ~value() =
    { pos; min_w; min_h; max_w; max_h; resizable; draggable; selectable;
      on_resize; on_resizing; on_drag; on_dragging;
      move_widget; widget; value}

  class ['a] cell ?(typ=`Item)
             ~s_col_w
             ~s_row_h
             ~s_item_margin
             ~(item: 'a item)
             () =
    let elt = match typ with
      | `Item  -> Markup.Dynamic_grid.Item.create ()       |> Tyxml_js.To_dom.of_element
      | `Ghost -> Markup.Dynamic_grid.Item.create_ghost () |> Tyxml_js.To_dom.of_element
    in
    let s_pos, s_pos_push = React.S.create item.pos in

    object(self)

      inherit Widget.widget elt ()

      val mutable px_pos = Position.empty

      (** API **)

      method pos        : Position.t              = React.S.value s_pos
      method s_pos      : Position.t React.signal = s_pos
      method set_pos    : Position.t -> unit      = s_pos_push ?step:None

      (** Private methods **)

      method private px_pos = px_pos

      method private set_x x =
        px_pos <- { px_pos with x = x + (fst @@ React.S.value s_item_margin) };
        self#root##.style##.transform := Js.string @@ Utils.translate px_pos.x px_pos.y
      method private set_y y =
        px_pos <- { px_pos with y = y + (snd @@ React.S.value s_item_margin) };
        self#root##.style##.transform := Js.string @@ Utils.translate px_pos.x px_pos.y
      method private set_w w =
        px_pos <- { px_pos with w = w - (fst @@ React.S.value s_item_margin) };
        self#root##.style##.width := Js.string @@ Utils.px px_pos.w
      method private set_h h =
        px_pos <- { px_pos with h = h - (snd @@ React.S.value s_item_margin) };
        self#root##.style##.height := Js.string @@ Utils.px px_pos.h

      initializer
        React.S.l4 (fun (pos:Position.t) w h _ ->
            self#set_x (pos.x * w);
            self#set_y (pos.y * h);
            self#set_w (pos.w * w);
            self#set_h (pos.h * h)) self#s_pos s_col_w s_row_h s_item_margin
        |> ignore

    end

  let eq x y = Equal.physical x#root y#root

  let filter ~(exclude:#Widget.widget list) (l:#Widget.widget list) =
    List.filter (fun x -> not (List.mem ~eq x exclude)) l

  class ['a] t ~grid          (* grid props *)
             ~(item: 'a item) (* item props *)
             ~e_modify_push   (* add/delete item event *)
             ~s_selected      (* selected items *)
             ~s_selected_push (* selected items signal modifier *)
             ~s_col_w         (* column width signal -- px *)
             ~s_row_h         (* row height signal   -- px *)
             ~s_item_margin   (* item margin         -- px *)
             ~(s_items : 'a t list React.signal) (* items signal *)
             () =
  object(self)

    inherit ['a] cell ~typ:`Item ~s_col_w ~s_row_h ~s_item_margin ~item () as super

    val s_change      = React.S.create item.pos
    val resize_button = Markup.Dynamic_grid.Item.create_resize_button ()
                        |> Tyxml_js.To_dom.of_element |> Widget.create

    val mutable mov_listener  = None
    val mutable end_listener  = None
    val mutable value         = item.value
    val mutable draggable     = item.draggable
    val mutable resizable     = item.resizable
    val mutable selectable    = item.selectable
    val mutable selected      = false
    val mutable drag_timer    = None

    val ghost = new cell ~typ:`Ghost ~s_col_w ~s_row_h ~s_item_margin ~item ()

    (** API **)
    method set_pos pos = super#set_pos pos; ghost#set_pos pos

    method s_changing = ghost#s_pos
    method s_change   = fst s_change

    method set_value (x:'a) = value <- x
    method get_value : 'a   = value

    method set_draggable (x : bool) = draggable <- x
    method get_draggable : bool     = draggable

    method set_resizable (x : bool) =
      if x
      then Dom.appendChild self#root resize_button#root
      else (try Dom.removeChild self#root resize_button#root; with _ -> ());
      resizable <- x
    method get_resizable : bool = resizable

    method set_selectable x = if not x then self#set_selected false;
                              selectable <- x
    method get_selectable   = selectable

    method remove : unit =
      self#set_selected false;
      e_modify_push (`Remove self)

    method set_selected x : unit =
      let o = React.S.value s_selected in
      match x with
      | true  -> let i = (self :> 'a t) in
                 self#add_class Markup.Dynamic_grid.Item.selected_class;
                 if grid.multi_select
                 then (if not self#get_selected then s_selected_push (i :: o))
                 else (List.iter (fun x -> if not @@ eq x self
                                           then x#remove_class Markup.Dynamic_grid.Item.selected_class) o;
                       s_selected_push [(self :> 'a t)]);
      | false -> self#remove_class Markup.Dynamic_grid.Item.selected_class;
                 if grid.multi_select
                 then (if self#get_selected
                       then (let n = List.filter (fun x -> not @@ eq x self) o in
                             s_selected_push n))
                 else s_selected_push []
    method get_selected   = selected

    (** Private methods **)

    method private get_drag_target = match item.move_widget with
      | Some w -> w
      | None   -> (self :> Widget.widget)

    method private items = React.S.value s_items

    method private has_collision (pos : Position.t) =
      Position.has_collision ~f:(fun x -> React.S.value x#s_pos)
                             pos
                             (List.filter (fun x -> x#root != self#root) (React.S.value s_items))

    method private get_parent : Dom_html.element Js.t =
      let par_opt = Js.Opt.to_option self#root##.parentNode |> Option.map Js.Unsafe.coerce in
      Option.get_exn par_opt

    method private get_parent_pos : Position.t =
      let par = self#get_parent in
      { x = par##.offsetLeft
      ; y = par##.offsetTop
      ; w = par##.offsetWidth
      ; h = par##.offsetHeight
      }

    method private mouse_action meth ev =
      let init_pos = px_pos in
      let init_x, init_y = ev##.clientX, ev##.clientY in
      Dom_events.listen Dom_html.window Dom_events.Typ.mousemove
                        (fun _ ev ->
                          let x, y = ev##.clientX, ev##.clientY in
                          meth ~x ~y ~init_x ~init_y ~init_pos `Move;
                          false)
      |> (fun x -> mov_listener <- Some x);
      Dom_events.listen Dom_html.window Dom_events.Typ.mouseup
                        (fun _ ev ->
                          let x, y = ev##.clientX, ev##.clientY in
                          meth ~x ~y ~init_x ~init_y ~init_pos `End;
                          false)
      |> (fun x -> end_listener <- Some x)

    method private touch_action meth ev =
      let init_pos = px_pos in
      Js.Optdef.iter (ev##.changedTouches##item 0)
                     ( fun touch ->
                       let id = touch##.identifier in
                       let init_x, init_y = touch##.clientX, touch##.clientY in
                       Dom_events.listen Dom_html.window Dom_events.Typ.touchmove
                                         (fun _ ev ->
                                           let length = ev##.changedTouches##.length - 1 in
                                           Js.Optdef.iter (ev##.changedTouches##item length)
                                                          (fun touch ->
                                                            let x, y = touch##.clientX, touch##.clientY in
                                                            if touch##.identifier = id then
                                                              meth ~x ~y ~init_x ~init_y ~init_pos `Move);
                                           false)
                       |> (fun x -> mov_listener <- Some x);
                       Dom_events.listen Dom_html.window Dom_events.Typ.touchend
                                         (fun _ ev ->
                                           let length = ev##.changedTouches##.length - 1 in
                                           Js.Optdef.iter (ev##.changedTouches##item length)
                                                          (fun touch ->
                                                            let x, y = touch##.clientX, touch##.clientY in
                                                            if touch##.identifier = id then
                                                              meth ~x ~y ~init_x ~init_y ~init_pos `End);
                                           false)
                       |> (fun x -> end_listener <- Some x))

    method private resolve_pos_conflicts ~action (pos : Position.t) =
      let other   = filter ~exclude:[(self :> 'a t)] self#items in
      let new_pos = match List.filter (fun x -> Position.collides pos x#pos) other with
        | [] -> pos
        | l  ->
           if not grid.vertical_compact
           then ghost#pos
           else
             let bind f    = function [] -> f () | l -> l in
             let (>>=) x f = bind f x in
             let check_top () = match action with
               | `Size -> []
               | `Drag -> Position.move_top
                            ~f:(fun x -> x#pos)
                            ~eq:(fun x y -> x#root != y#root)
                            ~collisions:l
                            pos other
             in
             let check_bot () = Position.move_down ?rows:grid.rows
                                                   ~f:(fun x -> x#pos)
                                                   ~eq:(fun x y -> x#root != y#root)
                                                   ~collisions:l pos other
             in
             let res = check_top ()
                       >>= check_bot
                       >>= (fun () -> [])
             in
             match res with
             | [] -> ghost#pos
             | l  -> List.iter (fun (pos,item) -> item#set_pos pos) l;
                     pos
      in
      (match action with
       | `Drag -> if grid.vertical_compact
                  then ghost#set_pos @@ Position.compact ~f:(fun x -> x#pos) new_pos other
                  else ghost#set_pos new_pos;
       | `Size -> ghost#set_pos new_pos);
      if grid.vertical_compact;
      then List.iter (fun x -> let lst = filter ~exclude:[(x:>'a t)] other
                                         |> List.map (fun x -> x#pos)
                                         |> List.cons ghost#pos
                               in
                               let pos = Position.compact ~f:(fun x -> x) x#pos lst in
                               x#set_pos pos)
                     (Position.sort_by_y ~f:(fun x -> x#pos) other)

    method private start_dragging (ev: action) =
      if self#get_draggable
      then
        (self#get_drag_target#add_class Markup.Dynamic_grid.Item.dragging_class;
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
            let x, y  = init_pos.x + x - init_x, init_pos.y + y - init_y in
            let pos   = Position.correct_xy { self#pos with x = x // col_px
                                                          ; y = y // row_px }
                                            grid.cols grid.rows in
            self#resolve_pos_conflicts ~action:`Drag pos;
            let x,y = if grid.restrict_move
                      then (let pos = Position.correct_xy { self#px_pos with x;y }
                                                          (grid.cols * col_px)
                                                          (Option.map (fun x -> x * row_px) grid.rows)
                            in
                            pos.x, pos.y)
                      else x,y
            in
            self#set_x x; self#set_y y;
            Option.iter (fun f -> f self#pos ghost#pos col_px row_px) item.on_dragging
         | `End->
            self#get_drag_target#remove_class Markup.Dynamic_grid.Item.dragging_class;
            Option.iter (fun l -> Dom_events.stop_listen l) mov_listener;
            Option.iter (fun l -> Dom_events.stop_listen l) end_listener;
            self#style##.zIndex := Js.string "";
            (* update element position from ghost *)
            self#set_x @@ React.S.value s_col_w * ghost#pos.x;
            self#set_y @@ React.S.value s_row_h * ghost#pos.y;
            self#set_pos ghost#pos;
            Dom.removeChild self#get_parent ghost#root;
            if grid.vertical_compact
            then List.iter (fun x -> let lst = filter ~exclude:[(x:>'a t)] self#items in
                                     let pos = Position.compact ~f:(fun x -> x#pos) x#pos lst in
                                     x#set_pos pos)
                           (Position.sort_by_y ~f:(fun x -> x#pos) self#items);
            (snd s_change) self#pos;
            Option.iter (fun f -> f self#pos ghost#pos col_px row_px) item.on_drag
         | _ -> ()

    method private start_resizing (ev: action) =
      if self#get_resizable
      then
        (ghost#style##.zIndex := Js.string "4";
         self#style##.zIndex  := Js.string "3";
         Dom.appendChild self#get_parent ghost#root;
         (* add resize/stop resize event listeners *)
         match ev with
         | Mouse ev -> Dom_html.stopPropagation ev;
                       self#mouse_action self#apply_size ev
         | Touch ev -> Dom_html.stopPropagation ev;
                       self#touch_action self#apply_size ev)


    method private apply_size ~x ~y ~init_x ~init_y ~init_pos typ =
      let col_px, row_px = React.S.value s_col_w, React.S.value s_row_h in
      match typ with
      | `Move ->
         let open Utils in
         let w, h = init_pos.w + x - init_x, init_pos.h + y - init_y in
         let pos  = Position.correct_wh ?max_w:item.max_w
                                        ?min_w:item.min_w
                                        ?max_h:item.max_h
                                        ?min_h:item.min_h
                                        { self#pos with w = w // col_px;
                                                        h = h // row_px }
                                        grid.cols
                                        grid.rows
         in
         self#resolve_pos_conflicts ~action:`Size pos;
         let w,h = if grid.restrict_move
                   then (let pos = Position.correct_wh { self#px_pos with x;y }
                                                       (grid.cols * col_px)
                                                       (Option.map (fun x -> x * row_px) grid.rows)
                         in
                         pos.w, pos.h)
                   else w,h
         in
         self#set_w w; self#set_h h;
         Option.iter (fun f -> f self#pos ghost#pos col_px row_px) item.on_resizing
      | `End ->
         Option.iter (fun l -> Dom_events.stop_listen l) mov_listener;
         Option.iter (fun l -> Dom_events.stop_listen l) end_listener;
         self#style##.zIndex := Js.string "";
         (* update element position from ghost *)
         self#set_w @@ React.S.value s_col_w * ghost#pos.w;
         self#set_h @@ React.S.value s_row_h * ghost#pos.h;
         self#set_pos ghost#pos;
         Dom.removeChild self#get_parent ghost#root;
         if grid.vertical_compact
         then List.iter (fun x -> let lst = filter ~exclude:[(x:>'a t)] self#items in
                                  let pos = Position.compact ~f:(fun x -> x#pos) x#pos lst in
                                  x#set_pos pos)
                        (Position.sort_by_y ~f:(fun x -> x#pos) self#items);
         (snd s_change) self#pos;
         Option.iter (fun f -> f self#pos ghost#pos col_px row_px) item.on_resize
      | _ -> ()

    initializer
      (* append resize button to element if necessary *)
      if item.resizable
      then Dom.appendChild self#root resize_button#root;
      (* append widget to cell if provided *)
      Option.iter (fun x -> Dom.appendChild self#root x#root) item.widget;
      (* add item move listener *)
      let select_target = (self :> Widget.widget) in
      if draggable
      then self#get_drag_target#add_class Markup.Dynamic_grid.Item.drag_handle_class;
      if selectable
      then select_target#add_class Markup.Dynamic_grid.Item.select_handle_class;

      Dom_events.listen self#get_drag_target#root Dom_events.Typ.mousedown
                        (fun _ e -> if e##.button = 0 && draggable then self#start_dragging (Mouse e); true)
      |> ignore;

      Dom_events.listen self#get_drag_target#root Dom_events.Typ.touchstart
                        (fun _ e -> if draggable then self#start_dragging (Touch e); false)
      |> ignore;

      (* Dom_events.listen select_target#root Dom_events.Typ.click
       *                   (fun _ _ -> Option.iter (fun tmr -> Dom_html.clearTimeout tmr) drag_timer;
       *                               if selectable
       *                               then (if grid.multi_select
       *                                     then self#set_selected @@ not self#get_selected
       *                                     else self#set_selected true);
       *                               true)
       * |> ignore; *)

      (* add item start resize listener if needed *)
      Dom_events.listen resize_button#root Dom_events.Typ.mousedown
                        (fun _ e -> if e##.button = 0 && resizable
                                    then self#start_resizing (Mouse e);
                                    false)
      |> ignore;

      Dom_events.listen resize_button#root Dom_events.Typ.touchstart
                        (fun _ e -> if resizable then self#start_resizing (Touch e); true)
      |> ignore

  end

end

type add_error = Collides  of Position.t list
               | Cancelled
               | In_progress

class ['a] t ~grid ~(items:'a item list) () =
  let e_modify,e_modify_push     = React.E.create () in
  let s_selected,s_selected_push = React.S.create [] in
  let s_col_w,s_col_w_push       = React.S.create grid.min_col_width in
  let s_row_h                    = match grid.row_height with
    | Some rh -> React.S.const rh
    | None    -> s_col_w
  in
  let s_item_margin,s_item_margin_push = React.S.create (Option.get_or ~default:(0,0) grid.items_margin) in
  let s_items  = React.S.fold
                   (fun acc x ->
                     match x with
                     | `Add x    -> x :: acc
                     | `Remove x -> List.filter (fun i -> i#root != x#root) acc)
                   [] e_modify in
  let new_item item = new Item.t ~grid ~s_items ~e_modify_push ~s_selected ~s_selected_push
                          ~s_col_w ~s_row_h ~s_item_margin ~item ()
  in
  let items    = List.map (fun item -> new_item item) items in
  let s_change =
    let m a x = x :: a in
    React.S.map (fun l -> React.S.merge m [] (List.map (fun x -> x#s_change) l)) s_items
    |> React.S.switch
  in
  (* FIXME *)
  let s_changing =
    let m a x = x :: a in
    React.S.map (fun l -> React.S.merge m [] (List.map (fun x -> x#s_changing) l)) s_items
    |> React.S.switch
  in
  let s_rows = match grid.rows with
    | Some h -> React.S.const h
    | None   ->
       let merge = (fun acc (x:Position.t) -> if (x.h + x.y) > acc then (x.h + x.y) else acc) in
       React.S.map (fun (l:Position.t list) -> List.fold_left merge 1 l) s_changing
  in
  let elt = Markup.Dynamic_grid.create ~items:[] () |> Tyxml_js.To_dom.of_element in

  object(self)

    inherit Widget.widget elt ()

    val mutable in_action = false
    val mutable in_dom    = false
    val mutable residue   = 0

    (** API **)

    method s_changing = s_changing
    method s_change   = s_change
    method s_items    = s_items

    method s_selected : 'a Item.t list React.signal = s_selected

    method items      = Position.sort_by_y ~f:(fun x -> x#pos) @@ React.S.value s_items
    method positions  = React.S.value s_change

    method get_item_margin = React.S.value s_item_margin
    method set_item_margin margin = s_item_margin_push margin

    method add (x:'a item) =
      let items = List.map (fun x -> x#pos) (React.S.value s_items) in
      match Position.get_all_collisions ~f:(fun x -> x) x.pos items with
      | [] -> let item = new_item x in
              e_modify_push (`Add item);
              Dom.appendChild self#root item#root;
              (* FIXME make vertical compact variable *)
              if grid.vertical_compact then self#compact;
              Ok ()
      | l  -> Error (Collides l)

    method add_free ?min_w ?min_h ?max_w ?max_h
                    ?(resizable=true)
                    ?(draggable=true)
                    ?(selectable=true)
                    ?on_resize ?on_resizing ?on_drag ?on_dragging
                    ?widget
                    ?(move_widget: Widget.widget option)
                    ?(width:int option)
                    ?(height:int option)
                    ~(value: 'a)
                    () =
      let item          = { pos = Position.empty
                          ; min_w; min_h; max_w; max_h
                          ; resizable; draggable; selectable
                          ; on_resize; on_resizing; on_drag; on_dragging
                          ; move_widget; widget; value }
      in
      let items         = List.map (fun x -> x#pos) @@ React.S.value s_items in
      let on_init _     = () in
      let on_move ghost = function
        | None      -> ghost#set_pos Position.empty
        | Some epos -> let epos =
                         Position.({ epos with x = epos.x / React.S.value s_col_w;
                                               y = epos.y / React.S.value s_row_h })
                       in
                       let open Position in
                       let cmp =
                         match width, height with
                         | Some w, Some h ->
                            Some (fun n o -> if n.w < w || n.h < h || (n.w * n.h) < (o.w * o.h)
                                             then 0 else 1)
                         | Some w, _      -> Some (fun n o -> if n.w < w || n.h < o.h then 0 else 1)
                         | _, Some h      -> Some (fun n o -> if n.h < h || n.w < o.w then 0 else 1)
                         | _              -> None
                       in
                       let pos = get_free_rect ?cmp ~f:(fun x -> x) epos items grid.cols
                                               (React.S.value s_rows) () in
                       let pos = Option.map (fun pos ->
                                     let corr_x = fun w -> if epos.x + w > pos.x + pos.w
                                                           then (pos.x + pos.w) - w
                                                           else epos.x
                                     in
                                     let corr_y = fun h -> if epos.y + h > pos.y + pos.h
                                                           then (pos.y + pos.h) - h
                                                           else epos.y
                                     in
                                     match width, height with
                                     | Some w, Some h ->
                                        let w = if w < 1 then 1 else w in
                                        let h = if h < 1 then 1 else h in
                                        { x = corr_x w; y = corr_y h; w; h}
                                     | Some w, _  -> let w = if w < 1 then 1 else w in
                                                     {pos with x = corr_x w; w}
                                     | _, Some h  -> let h = if h < 1 then 1 else h in
                                                     {pos with y = corr_y h; h}
                                     | _          -> pos) pos
                       in
                       (match pos with
                        | Some x -> ghost#set_pos x
                        | None   -> ghost#set_pos Position.empty)
      in
      let open Position in
      let on_click ghost = match ghost#pos with
        | x when x.w = 0 || x.h = 0 -> Error (Collides [])
        | pos                       -> self#add { item with pos }
      in
      self#action_wrapper ~on_init ~on_move ~on_click

    method remove (x:'a Item.t) vc =
      x#remove;
      (* FIXME make vertical compact variable *)
      if grid.vertical_compact && vc then self#compact

    method remove_all () = List.iter (fun x -> self#remove x false) self#items

    method remove_free =
      let items         = List.map (fun x -> x#pos) @@ React.S.value s_items in
      let on_init ghost = ghost#style##.zIndex := Js.string "3" in
      let on_move ghost = function
        | None      -> ghost#set_pos Position.empty
        | Some epos -> let epos =
                         Position.({ epos with x = epos.x / React.S.value s_col_w;
                                               y = epos.y / React.S.value s_row_h
                         })
                       in
                       begin
                         match Position.get_first_collision ~f:(fun x -> x) epos items with
                         | Some x  -> ghost#set_pos x
                         | None    -> ghost#set_pos Position.empty
                       end
      in
      let on_click ghost =
        let el = List.fold_while (fun acc x -> if Position.collides ghost#pos x#pos
                                               then (Some x, `Stop)
                                               else (acc, `Continue)) None (React.S.value self#s_items)
        in
        match el with
        | Some x -> self#remove x true; Ok ()
        | None   -> Error (Collides [])
      in
      self#action_wrapper ~on_init ~on_move ~on_click

    method layout =
      let w   = self#get_offset_width + residue - (fst self#get_item_margin) in
      let col = w / grid.cols in
      let res = w mod grid.cols in
      s_col_w_push col;
      residue <- res;
      self#style##.width := Js.string @@ Printf.sprintf "calc(100%% - %dpx)" res

    (** Private methods **)

    method private compact =
      let sorted = Position.sort_by_y ~f:(fun x -> x#pos) self#items in
      let other (i:'a Item.t) = List.filter (fun x -> not @@ Equal.physical x#root i#root) self#items in
      List.iter (fun (x:'a Item.t) -> let new_pos = Position.compact ~f:(fun x -> x#pos) x#pos (other x) in
                                      x#set_pos new_pos)
                sorted

    method private get_event_pos e : Position.t option =
      let rect = self#get_client_rect in
      let left = int_of_float rect.left in
      let top  = int_of_float rect.top in
      let x = e##.clientX - left in
      let y = e##.clientY - top  in
      let (pos:Position.t) = { x; y; w = 1; h = 1 } in
      if x <= self#get_offset_width && x >= 0 && y <= self#get_offset_height && y >= 0
      then Some pos else None

    method private action_wrapper ~on_init ~on_move ~on_click =
      match in_action with
      | true  -> Lwt.return_error In_progress
      | false ->
         in_action <- true;
         let open Lwt.Infix in
         let t,wakener = Lwt.wait () in
         let item      = Item.to_item ~pos:Position.empty ~min_w:1 ~min_h:1 ~max_w:1 ~max_h:1 ~value:() () in
         let ghost     = new Item.cell ~typ:`Ghost ~s_col_w ~s_row_h ~s_item_margin ~item () in
         on_init ghost;
         Dom.appendChild self#root ghost#root;
         let move_listener  =
           Dom_events.listen Dom_html.document##.body
                             Dom_events.Typ.mousemove
                             (fun _ e -> let epos = self#get_event_pos e in on_move ghost epos; true)
         in
         let click_listener =
           Dom_events.listen self#root
                             Dom_events.Typ.click
                             (fun _ _ -> Lwt.wakeup wakener @@ on_click ghost; false)
         in
         let esc_listener   =
           Dom_events.listen Dom_html.window
                             Dom_events.Typ.keydown
                             (fun _ e -> let key = Option.map Js.to_string @@ Js.Optdef.to_option e##.key in
                                         (match key,e##.keyCode with
                                          | Some "Esc",_ | Some "Escape",_ | _,27 ->
                                             Lwt.wakeup wakener @@ Error Cancelled
                                          | _      -> ());
                                         true)
         in
         t >>= (fun _ -> in_action <- false;
                         Dom_events.stop_listen move_listener;
                         Dom_events.stop_listen click_listener;
                         Dom_events.stop_listen esc_listener;
                         Dom.removeChild self#root ghost#root;
                         (* FIXME make vertical compact variable *)
                         if grid.vertical_compact then self#compact;
                         Lwt.return_unit) |> ignore;
         t

    initializer
      (* add item add/remove listener *)
      React.E.map (function
          | `Add (x:'a Item.t) -> Dom.appendChild self#root x#root
          | `Remove x          -> Dom.removeChild self#root x#root) e_modify
      |> ignore;
      (* add initial items *)
      List.iter (fun x -> e_modify_push (`Add x)) items;
      (* add min/max width update listener *)
      React.S.map (fun margin ->
          let m_top = snd margin in
          self#style##.minWidth := Js.string @@ Utils.px (grid.cols * grid.min_col_width + m_top);
          Option.iter (fun x -> self#style##.maxWidth := Js.string @@ Utils.px @@ grid.cols * x + m_top)
                      grid.max_col_width)
                  s_item_margin
      |> ignore;
      (* add height update listener *)
      React.S.l3 (fun h row_h margin -> self#style##.height := Js.string @@ Utils.px (h * row_h + (snd margin)))
                 s_rows s_row_h s_item_margin
      |> ignore;
      Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ -> self#layout; true)
      |> ignore;

  end
