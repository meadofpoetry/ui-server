open Containers

module Utils = struct

  (** rounds a number the right way **)
  let round x =
    if Float.(x < (floor x +. 0.5))
    then int_of_float @@ floor x
    else int_of_float @@ ceil x

  let px n = Js.string @@ Printf.sprintf "%dpx" n

  let (//) x y =
    round @@ (float_of_int x) /. (float_of_int y)

    type what_to_do = | Add | Remove
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

  (** checks if two elements collide, returns true if do and false otherwise **)
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
      let items    = CCList.map f items in
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
  { pos       : Position.t
  ; min_w     : int option
  ; min_h     : int option
  ; max_w     : int option
  ; max_h     : int option
  ; static    : bool
  ; resizable : bool
  ; draggable : bool
  ; widget    : Widget.widget option
  ; value     : 'a
  }

type grid =
  { min_col_width    : int
  ; max_col_width    : int option
  ; cols             : int
  ; rows             : int option
  ; row_height       : int option
  ; vertical_compact : bool
  ; items_margin     : int option
  }

module Item = struct

  type action = Mouse of Dom_html.mouseEvent Js.t
              | Touch of Dom_html.touchEvent Js.t

  let to_item ?min_w ?min_h ?max_w ?max_h
        ?(static=false) ?(resizable=true) ?(draggable=true) ?widget ~pos ~value() =
    { pos; min_w; min_h; max_w; max_h; static; resizable; draggable; widget; value}

  class ['a] cell ?(typ=`Item)
          ~s_col_w
          ~s_row_h
          ~(item: 'a item)
          () =
    let elt = match typ with
      | `Item  -> Markup.Dynamic_grid.Item.create ()       |> Tyxml_js.To_dom.of_element
      | `Ghost -> Markup.Dynamic_grid.Item.create_ghost () |> Tyxml_js.To_dom.of_element
    in
    let s_pos, s_pos_push = React.S.create item.pos in

    object(self)

      inherit Widget.widget elt ()

      (** API **)

      method pos        : Position.t              = React.S.value s_pos
      method s_pos      : Position.t React.signal = s_pos
      method s_pos_push : Position.t -> unit      = s_pos_push ?step:None

      (** Private methods **)

      method private set_x x = self#root##.style##.left   := Utils.px x
      method private set_y y = self#root##.style##.top    := Utils.px y
      method private set_w w = self#root##.style##.width  := Utils.px w
      method private set_h h = self#root##.style##.height := Utils.px h

      initializer
        React.S.l3 (fun (pos:Position.t) w h ->
            self#set_x (pos.x * w);
            self#set_y (pos.y * h);
            self#set_w (pos.w * w);
            self#set_h (pos.h * h)) self#s_pos s_col_w s_row_h
        |> ignore

    end

  class ['a] t ~grid          (* grid props *)
          ~(item: 'a item)    (* item props *)
          ~e_modify_push (* add/delete item event *)
          ~s_col_w       (* column width signal -- px *)
          ~s_row_h       (* row height signal   -- px *)
          ~(s_items : 'a t list React.signal) (* items signal *)
          () =
  object(self: 'self)

    inherit ['a] cell ~typ:`Item ~s_col_w ~s_row_h ~item ()


    val resize_button = Markup.Dynamic_grid.Item.create_resize_button ()
                        |> Tyxml_js.To_dom.of_element |> Widget.create

    val mutable dragged       = false
    val mutable mov_listener  = None
    val mutable end_listener  = None
    val mutable value         = item.value
    val mutable static        = item.static

    val ghost = new cell ~typ:`Ghost ~s_col_w ~s_row_h ~item ()

    (** API **)
    method set_value (x:'a) = value <- x
    method get_value : 'a   = value

    method set_static (s: bool) = static <- s
    method get_static : bool    = static

    method dragged : bool   = dragged

    method ghost         = ghost
    method remove : unit = e_modify_push (`Remove (self: 'self))

    method sort_by_height (list: 'a t list) =
      let open Position in
      let compare t1 t2 =
        if t1#pos.y = t2#pos.y then 0
        else if t1#pos.y > t2#pos.y then 1
        else -1 in
      List.sort compare @@ list

    method move_away (y : int) : bool =
      let should_move =
        match grid.rows with
        | Some rows ->
           if y + self#pos.h <= rows
           then CCList.fold_while
                  ( fun acc x ->
                    let open Position in
                    let new_bound = y + self#pos.h + x#pos.h in
                    if Position.collides x#pos {self#pos with y}
                    then
                      if new_bound <= rows
                      then ( if x#move_away @@ y + self#pos.h
                             then (acc, `Continue)
                             else (false, `Stop))
                      else (false, `Stop)
                    else (acc, `Continue)) true self#get_other_items
           else false
        | None ->
           CCList.fold_while
             ( fun acc x -> if x#move_away @@ self#pos.y + self#pos.h
                            then (acc, `Continue)
                            else (false, `Stop)) true self#get_other_items in
      if should_move
      then
        ( self#s_pos_push {self#pos with y};
          self#set_y @@ React.S.value s_row_h * y;
          true )
      else false

   method vc (pos: Position.t) : Position.t =
      let rec up (pos: Position.t) =
        let positions_except_current = List.map (fun x -> x#pos) self#get_other_items in
        if pos.y - 1 >= 0 &&
             (not @@ Position.has_collision
                       ~f:(fun x -> x)
                       {pos with y = pos.y - 1}
                       positions_except_current)
        then up {pos with y = pos.y - 1}
        else pos in
      up pos

    method get_other_items : 'a t list =
      List.filter (fun x -> x#root != self#root) @@ React.S.value s_items

    (** Private methods **)

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
      ; h = par##.offsetHeight }

    method private get_px_pos : Position.t =
      {  x = self#get_offset_left;  y = self#get_offset_top
       ; w = self#get_offset_width; h = self#get_offset_height }

    method private mouse_action meth ev =
      let init_pos = self#get_px_pos in
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
      let init_pos = self#get_px_pos in
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

    method private start_dragging (ev: action) =
      if not self#get_static then
        (
          dragged <- true;
          self#add_class Markup.Dynamic_grid.Item.dragging_class;
          ghost#style##.zIndex := Js.string "1";
          (* add ghost item to dom to show possible element position *)
          Dom.appendChild self#get_parent ghost#root;
          match ev with
          | Mouse ev -> self#mouse_action self#apply_position ev
          | Touch ev -> self#touch_action self#apply_position ev )


    method private apply_position ~x ~y ~init_x ~init_y ~init_pos typ =
      match x, y with
      | 0,0 -> ()
      | _   ->
         match typ with
         | `Move->
            let open Utils in
            let col_px, row_px = React.S.value s_col_w, React.S.value s_row_h in
            let x, y = init_pos.x + x - init_x, init_pos.y + y - init_y in
            let pos = Position.correct_xy { self#pos with x = x // col_px
                                                        ; y = y // row_px }
                        grid.cols grid.rows in
            if grid.vertical_compact
            then
              if not (self#has_collision pos)
              then ghost#s_pos_push @@ self#vc pos
              else ( let collisions =
                       List.filter (fun x -> Position.collides pos x#pos)
                         @@ self#get_other_items in
                     let continue =
                       List.fold_while (fun acc x ->
                           if x#move_away @@ pos.y + pos.h
                           then (acc, `Continue)
                           else (false, `Stop)) true collisions in
                     if continue
                     then ghost#s_pos_push @@ self#vc pos)
            else
              if not (self#has_collision pos)
              then ghost#s_pos_push pos;
            self#s_pos_push ghost#pos;
            self#set_x x;
            self#set_y y;
            if grid.vertical_compact
            then List.iter (fun x -> x#s_pos_push @@ x#vc x#pos)
                 @@ self#sort_by_height self#get_other_items
         | `End->
            self#remove_class Markup.Dynamic_grid.Item.dragging_class;
            Option.iter (fun l -> Dom_events.stop_listen l) mov_listener;
            Option.iter (fun l -> Dom_events.stop_listen l) end_listener;
            (* update element position from ghost *)
            self#set_x @@ React.S.value s_col_w * ghost#pos.x;
            self#set_y @@ React.S.value s_row_h * ghost#pos.y;
            self#s_pos_push ghost#pos;
            Dom.removeChild self#get_parent ghost#root;
            dragged <- false;
            if grid.vertical_compact
            then List.iter (fun x -> x#s_pos_push @@ x#vc x#pos)
                 @@ self#sort_by_height self#get_other_items
         | _ -> ()

    method private start_resizing (ev: action) =
      if not self#get_static then
        (
          dragged <- true;
          ghost#style##.zIndex := Js.string "3";
          Dom.appendChild self#get_parent ghost#root;
          (* add resize/stop resize event listeners *)
          match ev with
          | Mouse ev -> Dom_html.stopPropagation ev;
                        self#mouse_action self#apply_size ev
          | Touch ev -> Dom_html.stopPropagation ev;
                        self#touch_action self#apply_size ev)

    method private apply_size ~x ~y ~init_x ~init_y ~init_pos typ =
      match typ with
      | `Move ->
         let open Utils in
         let col_px, row_px = React.S.value s_col_w, React.S.value s_row_h in
         let w, h   = init_pos.w + x - init_x, init_pos.h + y - init_y in
         let pos = Position.correct_wh ?max_w:item.max_w
                     ?min_w:item.min_w
                     ?max_h:item.max_h
                     ?min_h:item.min_h
                     { self#pos with w = w // col_px;
                                     h = h // row_px }
                     grid.cols
                     grid.rows
         in
         if grid.vertical_compact
         then
           if not (self#has_collision pos)
           then ghost#s_pos_push @@ self#vc pos
           else ( let collisions =
                    List.filter (fun x -> Position.collides pos x#pos)
                    @@ self#get_other_items in
                  let continue =
                    List.fold_while (fun acc x ->
                        if x#move_away @@ pos.y + pos.h
                        then (acc, `Continue)
                        else (false, `Stop)) true collisions in
                  if continue
                  then ghost#s_pos_push @@ self#vc pos)
         else
           if not (self#has_collision pos)
           then ghost#s_pos_push pos;
         self#s_pos_push ghost#pos;
         self#set_w w;
         self#set_h h;
         if grid.vertical_compact
         then List.iter (fun x -> x#s_pos_push @@ x#vc x#pos)
              @@ self#sort_by_height self#get_other_items
      | `End ->
         Option.iter (fun l -> Dom_events.stop_listen l) mov_listener;
         Option.iter (fun l -> Dom_events.stop_listen l) end_listener;
         (* update element position from ghost *)
         self#set_w @@ React.S.value s_col_w * ghost#pos.w;
         self#set_h @@ React.S.value s_row_h * ghost#pos.h;
         self#s_pos_push ghost#pos;
         Dom.removeChild self#get_parent ghost#root;
         dragged <- false;
         if grid.vertical_compact
         then List.iter (fun x -> x#s_pos_push @@ x#vc x#pos)
              @@ self#sort_by_height self#get_other_items
      | _ -> ()

    initializer
      (* append resize button to element if necessary *)
      if item.resizable && not item.static
      then Dom.appendChild self#root resize_button#root;
      (* append widget to cell if provided *)
      Option.iter (fun x -> Dom.appendChild self#root x#root) item.widget;

      (* add item move listener *)
      Dom_events.listen self#root Dom_events.Typ.mousedown
        (fun _ e -> if item.draggable then self#start_dragging (Mouse e); false)
      |> ignore;

      Dom_events.listen self#root Dom_events.Typ.touchstart
        (fun _ e -> if item.draggable then self#start_dragging (Touch e); false)
      |> ignore;

      (* add item start resize listener if needed *)
      Dom_events.listen resize_button#root Dom_events.Typ.mousedown
        (fun _ e -> if item.resizable then self#start_resizing (Mouse e); false)
      |> ignore;

      Dom_events.listen resize_button#root Dom_events.Typ.touchstart
        (fun _ e -> if item.resizable then self#start_resizing (Touch e); false)
      |> ignore

  end

end

type add_error = Collides  of Position.t list
               | Cancelled
               | In_progress

class ['a] t ~grid ~(items:'a item list) () =
  let e_modify,e_modify_push = React.E.create () in
  let s_col_w,s_col_w_push   = React.S.create grid.min_col_width in
  let s_row_h,_              = match grid.row_height with
    | Some rh -> React.S.create rh
    | None    -> s_col_w, s_col_w_push
  in
  (* let s_row_h,s_row_h_push = React.S.create 0 in *)
  let s_items  = React.S.fold
                   (fun acc x ->
                     match x with
                     | `Add x    -> x :: acc
                     | `Remove x -> List.filter (fun i -> i#root != x#root) acc)
                   [] e_modify in
  let items    = List.map
                   (fun item -> new Item.t ~grid ~s_items ~e_modify_push ~s_col_w ~s_row_h ~item ())
                   items
  in
  let s_change =
    let m a x = x :: a in
    React.S.map (fun l -> React.S.merge m [] (List.map (fun x -> x#s_pos) l)) s_items
    |> React.S.switch
  in
  let s_changing =
    let m a x = x :: a in
    React.S.map (fun l -> React.S.merge m [] (List.map (fun x -> x#ghost#s_pos) l)) s_items
    |> React.S.switch
  in
  let s_rows = match grid.rows with
    | Some h -> React.S.const h
    | None   ->
       let merge = (fun acc (x:Position.t) -> if (x.h + x.y) > acc then (x.h + x.y) else acc) in
       React.S.map (fun (l:Position.t list) -> List.fold_left merge 0 l) s_changing
  in
  let elt = Markup.Dynamic_grid.create ~items:[] () |> Tyxml_js.To_dom.of_element in

  object(self)

    inherit Widget.widget elt ()

    val mutable adding   = false
    val mutable removing = false
    val mutable in_dom   = false
    val mutable residue  = 0

    (** API **)

    method s_changing = s_changing
    method s_change   = s_change
    method s_items    = s_items

    method items      = React.S.value s_items
    method positions  = React.S.value s_change

    method sorted_by_height =
      let open Position in
      let compare t1 t2 =
        if t1#pos.y = t2#pos.y then 0
        else if t1#pos.y > t2#pos.y then 1
        else -1 in
      List.sort compare @@ React.S.value s_items

    method vertical_compacting () =
      let sorted = self#sorted_by_height in
      List.iter (fun (x:'a Item.t) ->
          if not x#dragged then
            (
              let except_current_el = x#get_other_items in
              let positions = List.map (fun x -> x#pos) except_current_el in
              while (x#pos.y - 1 >= 0) &&
                      (not @@ Position.has_collision
                                ~f:(fun x -> x)
                                { x#pos with y = x#pos.y - 1}
                                positions)
              do x#s_pos_push { x#pos with y = x#pos.y - 1} done)) sorted

    method remove (x:'a Item.t) = x#remove; self#vertical_compacting ()

    method add (x:'a item) =
      let items = List.map (fun x -> x#pos) (React.S.value s_items) in
      match Position.get_all_collisions ~f:(fun x -> x) x.pos items with
      | [] -> let item =
                new Item.t ~grid ~e_modify_push ~s_col_w ~s_row_h ~s_items ~item:x () in
              e_modify_push (`Add item);
              Dom.appendChild self#root item#root;
              self#vertical_compacting ();
              Ok ()
      | l  -> Error (Collides l)

    method free ?min_w ?min_h ?max_w ?max_h
                    ?(static = false)
                    ?(resizable = true)
                    ?(draggable = true)
                    ?widget
                    ?(width: int option)
                    ?(height: int option)
                    ~(value: 'a)
                    ~(act: Utils.what_to_do)
                    () =
      let progress, item =
        match act with
        | Remove -> removing,
                    { pos       = Position.empty
                    ; min_w     = Some 1
                    ; min_h     = Some 1
                    ; max_w     = Some 1
                    ; max_h     = Some 1
                    ; static    = false
                    ; resizable = true
                    ; draggable = true
                    ; widget    = None
                    ; value }
        | Add -> adding,
                 { pos = Position.empty
                 ; min_w; min_h
                 ; max_w; max_h
                 ; static; resizable
                 ; draggable; widget
                 ; value }
      in
      if progress
      then Lwt.return_error In_progress
      else
        let open Lwt.Infix in
        adding <- true;
        let t,wakener = Lwt.wait () in
        let items = List.map (fun x -> x#pos) @@ React.S.value s_items in
        let ghost = new Item.cell ~typ:`Ghost ~s_col_w ~s_row_h ~item () in
        begin
          match act with
          | Remove ->
             ghost#style##.zIndex := Js.string "3"
          | Add -> ()
        end;
        Dom.appendChild self#root ghost#root;
        let mv_l  =
          Dom_events.listen
            Dom_html.document##.body
            Dom_events.Typ.mousemove
            (fun _ e ->
              (match act with
               | Add ->
                  (match self#get_event_pos e with
                   | Some ev_pos ->
                      let ev_pos = { ev_pos with x = ev_pos.x / React.S.value s_col_w;
                                                 y = ev_pos.y / React.S.value s_row_h }
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
                      let pos = get_free_rect ?cmp ~f:(fun x -> x) ev_pos items grid.cols
                                  (React.S.value s_rows) () in
                      let pos = Option.map (fun pos ->
                                    let corr_x = fun w -> if ev_pos.x + w > pos.x + pos.w
                                                          then (pos.x + pos.w) - w
                                                          else ev_pos.x
                                    in
                                    let corr_y = fun h -> if ev_pos.y + h > pos.y + pos.h
                                                          then (pos.y + pos.h) - h
                                                          else ev_pos.y
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
                       | Some x -> ghost#s_pos_push x
                       | None   -> ghost#s_pos_push Position.empty)
                   | None -> ghost#s_pos_push Position.empty)
               | Remove -> match self#get_event_pos e with
                           | Some ev_pos ->
                              let ev_pos =
                                { ev_pos with x = ev_pos.x / React.S.value s_col_w;
                                              y = ev_pos.y / React.S.value s_row_h
                                }
                              in
                              begin
                                match Position.get_first_collision ~f:(fun x -> x) ev_pos items with
                                | Some x  -> ghost#s_pos_push x
                                | None    -> ghost#s_pos_push Position.empty
                              end;
                           | None -> ghost#s_pos_push Position.empty);
              true)
        in
        let cl_l  =
          Dom_events.listen self#root
            Dom_events.Typ.click
            (fun _ _ ->
              (match act with
               | Add ->
                  begin match ghost#pos with
                  | x when x.w = 0 || x.h = 0 -> Lwt.wakeup wakener (Error (Collides []))
                  | pos -> Lwt.wakeup wakener (self#add { item with pos })
                  end
               | Remove ->
                  begin
                    let el = CCList.fold_while
                               (fun acc x ->
                                 if Position.collides ghost#pos x#pos
                                 then (Some x, `Stop)
                                 else (acc, `Continue)) None (React.S.value self#s_items) in
                    match el with
                    | Some x -> (self#remove x);
                                Lwt.wakeup wakener (Ok ())
                    | None   -> Lwt.wakeup wakener (Error (Collides []));
                  end);
              false)
        in
        let esc_l =
          Dom_events.listen Dom_html.window
            Dom_events.Typ.keydown
            (fun _ ev ->
              let key  = Option.map Js.to_string @@ Js.Optdef.to_option ev##.key in
              (match key,ev##.keyCode with
               | Some "Esc"     ,_
                 | Some "Escape",_
                 | _, 27 -> Lwt.wakeup wakener (Error (Collides []))
               | _      -> ());
              true)
        in
        t >>= (fun _ -> adding <- false;
                        Dom_events.stop_listen mv_l;
                        Dom_events.stop_listen cl_l;
                        Dom_events.stop_listen esc_l;
                        Dom.removeChild self#root ghost#root;
                        Lwt.return_unit) |> ignore;
        self#vertical_compacting ();
        t

    method layout =
      let w   = self#get_offset_width + residue in
      let col = w / grid.cols in
      let res = w mod grid.cols in
      s_col_w_push col;
      residue <- res;
      self#style##.width := Js.string @@ Printf.sprintf "calc(100%% - %dpx)" res

    (** Private methods *)

    method private get_event_pos e : Position.t option =
      let rect = self#get_client_rect in
      let left = int_of_float rect.left in
      let top  = int_of_float rect.top in
      let x = e##.clientX - left in
      let y = e##.clientY - top  in
      let (pos:Position.t) = { x; y; w = 1; h = 1 } in
      if x <= self#get_offset_width && x >= 0 && y <= self#get_offset_height && y >= 0
      then Some pos else None

    initializer
      (* set min/max width of grid *)
      self#style##.minWidth := Utils.px (grid.cols * grid.min_col_width);
      Option.iter (fun x -> self#style##.maxWidth := Utils.px @@ grid.cols * x) grid.max_col_width;
      (* add item add/remove listener *)
      React.E.map (function
          | `Add (x:'a Item.t) -> Dom.appendChild self#root x#root
          | `Remove x       -> Dom.removeChild self#root x#root) e_modify
      |> ignore;
      (* add initial items *)
      List.iter (fun x -> e_modify_push (`Add x)) items;
      (* add height update listener *)
      React.S.l2 (fun h row_h -> self#style##.height := Utils.px (h * row_h)) s_rows s_row_h
      |> ignore;
      Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ -> self#layout; true)
      |> ignore;
      MutationObserver.observe
        ~node:Dom_html.document
        ~f:(fun _ _ ->
          let in_dom_new = (Js.Unsafe.coerce Dom_html.document)##contains self#root in
          (match in_dom,in_dom_new with
           | false, true -> self#layout
           | _           -> ());
          in_dom <- in_dom_new)
        ~child_list:true
        ~subtree:true
        ()
      |> ignore

  end
