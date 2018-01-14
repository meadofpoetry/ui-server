module Utils = struct

  (** rounds a number the right way **)
  let round x =
    if x < (floor x +. 0.5)
    then int_of_float @@ floor x
    else int_of_float @@ ceil x

  let px n = Js.string @@ Printf.sprintf "%dpx" n

  let (//) x y =
    round @@ (float_of_int x) /. (float_of_int y)

  let (///) x y =
    int_of_float @@ ceil @@ (float_of_int x) /. (float_of_int y)

end

module Position = struct

  type t =
    { x : int
    ; y : int
    ; w : int
    ; h : int
    }

  let empty = { x=0;y=0;w=0;h=0 }

  (** checks if two elements collide, returns true if do and false otherwise **)
  let collides (pos1:t) (pos2:t) =
    if (pos1.x + pos1.w <= pos2.x)      then false
    else if (pos1.x >= pos2.x + pos2.w) then false
    else if (pos1.y + pos1.h <= pos2.y) then false
    else if (pos1.y >= pos2.y + pos2.h) then false
    else true

  (** get first element that collides with position **)
  let get_first_collision ~(f:'a -> t) pos (l:'a list) =
    CCList.fold_while (fun acc (x:'a) -> if collides pos (f x)
                                         then (Some x,`Stop)
                                         else (acc,`Continue)) None l

  (** get all elements that collides with position **)
  let get_all_collisions ~(f:'a -> t) pos (l:'a list) =
    CCList.fold_left (fun acc (x:'a) -> if collides pos (f x)
                                        then x::acc else acc) [] l

  (** check if element collides with other elements **)
  let has_collision ~(f:'a -> t) x (l:'a list) =
    CCOpt.is_some @@ get_first_collision ~f x l

  let get_free_rect ~(f:'a -> t) (pos:t) (items:'a list) w h =
    if has_collision ~f:(fun x -> x) pos items
    then None
    else
      let items = CCList.map f items in
      let area pos = pos.w * pos.h in
      (* FIXME obviously not optimized algorithm *)
      (* get only elements that are on the way to cursor proection to the left/right side *)
      let x_filtered = CCList.filter (fun i -> pos.y >= i.y && pos.y <= i.y + i.h) items in
      (* get cursor proection to the left side *)
      let l = CCList.filter (fun i -> i.x < pos.x) x_filtered
              |> CCList.fold_left (fun acc i -> if i.x + i.w > acc.x + acc.w then i else acc) empty
              |> (fun x -> x.x + x.w) in
      (* get cursor proection to the right side *)
      let r = CCList.filter (fun i -> i.x > pos.x) x_filtered
              |> CCList.fold_left (fun acc i -> if i.x < acc.x then i else acc) { x=w;y=0;w=0;h=0 }
              |> (fun x -> x.x) in
      (* get only elements that are on the way to cursor proection to the top/bottom side *)
      let y_filtered = CCList.filter (fun i -> pos.x >= i.x && pos.x <= i.x + i.w && i.x + i.w > l && i.x < r)
                                     items in
      (* get cursor proection to the top side *)
      let t = CCList.filter (fun i -> i.y < pos.y) y_filtered
              |> CCList.fold_left (fun acc i -> if i.y + i.h > acc.y + acc.h then i else acc) empty
              |> (fun x -> x.y + x.h) in
      (* get cursor proection to the bottom side *)
      let b = CCList.filter (fun i -> i.y > pos.y) y_filtered
              |> CCList.fold_left (fun acc i -> if i.y < acc.y then i else acc) { x=0;y=h;w=0;h=0}
              |> (fun x -> x.y) in
      (* get available x points, FIXME obviously we don't need to iterate over all items *)
      let xs = CCList.fold_left (fun acc i ->
                   let join = fun x lst -> if x >= l && x <= r then x :: lst else lst in
                   let acc  = join i.x acc |> join (i.x + i.w) in
                   acc) [] items
               |> (fun x -> l :: x @ [r])
               |> CCList.sort_uniq
      in
      (* get available y points, FIXME obviously we don't need to iterate over all items *)
      let ys = CCList.fold_left (fun acc i ->
                   let join = fun y lst -> if y >= t && y <= b then y :: lst else lst in
                   let acc  = join i.y acc |> join (i.y + i.h) in
                   acc) [] items
               |> (fun x -> t :: x @ [b])
               |> CCList.sort_uniq
      in
      (* get biggest non-overlapping rectangle under the cursor *)
      (* FIXME obviously not optimized at all *)
      let a,_ = CCList.fold_left (fun acc x0 ->
                    let xs = CCList.filter (fun i -> i > x0) xs in
                    CCList.fold_left (fun acc x1 ->
                        CCList.fold_left (fun acc y0 ->
                            let ys = CCList.filter (fun i -> i > y0) ys in
                            CCList.fold_left (fun acc y1 ->
                                let _,acc_area = acc in
                                let (new_pos:t) = { x = x0; y = y0; w = x1 - x0; h = y1 - y0 } in
                                let new_area = area new_pos in
                                (*
                                 * new rect must be the biggest one available,
                                 * it must not overlap with other rects,
                                 * it must be under the mouse cursor
                                 *)
                                match (new_area > acc_area),
                                      get_first_collision ~f:(fun x -> x) new_pos items,
                                      collides pos new_pos with
                                | true,None,true -> new_pos,new_area
                                | _         -> acc) acc ys) acc ys) acc xs)
                                 (empty,0) xs in
      Some a

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

type item =
  { pos       : Position.t
  ; min_w     : int option
  ; min_h     : int option
  ; max_w     : int option
  ; max_h     : int option
  ; static    : bool
  ; resizable : bool
  ; draggable : bool
  ; widget    : Widget.widget option
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

  open Position

  let to_item ?min_w ?min_h ?max_w ?max_h
                   ?(static=false) ?(resizable=true) ?(draggable=true) ?widget ~pos () =
    { pos; min_w; min_h; max_w; max_h; static; resizable; draggable; widget }

  class cell ?(typ=`Item)
             ~s_col_w
             ~s_row_h
             ~item
             () =
    let elt = match typ with
      | `Item  -> Markup.Dynamic_grid.Item.create () |> Tyxml_js.To_dom.of_element
      | `Ghost -> Markup.Dynamic_grid.Item.create_ghost () |> Tyxml_js.To_dom.of_element
    in
    let s_pos, s_pos_push = React.S.create item.pos in

    object(self)

      inherit Widget.widget elt ()

      (** API **)

      method pos        = React.S.value s_pos
      method s_pos      = s_pos
      method s_pos_push = s_pos_push

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

  let dragstart_str,dragstart_ev = "dragstart", Dom_events.Typ.dragstart
  let drag_str,drag_ev           = "drag",      Dom_events.Typ.drag
  let dragend_str,dragend_ev     = "dragend",   Dom_events.Typ.dragend

  (* let dragstart_str,dragstart_ev = "mousedown", Dom_events.Typ.mousedown
   * let drag_str,drag_ev           = "mousemove", Dom_events.Typ.mousemove
   * let dragend_str,dragend_ev     = "mouseup",   Dom_events.Typ.mouseup *)

  class t ~grid          (* grid props *)
          ~item          (* item props *)
          ~e_modify_push (* add/delete item event *)
          ~s_col_w       (* column width signal -- px *)
          ~s_row_h       (* row height signal   -- px *)
          ~s_items       (* items signal *)
          () =
  object(self)

    inherit cell ~typ:`Item ~s_col_w ~s_row_h ~item ()


    val resize_button = Markup.Dynamic_grid.Item.create_resize_button ()
                        |> Tyxml_js.To_dom.of_element |> Widget.create

    val mutable mov_listener = None
    val mutable end_listener = None
    val ghost = new cell ~typ:`Ghost ~s_col_w ~s_row_h ~item ()

    (** API **)

    method ghost         = ghost
    method remove : unit = e_modify_push (`Remove self)

    (** Private methods **)

    method private has_collision (pos : Position.t) =
      Position.has_collision ~f:(fun x -> React.S.value x#s_pos)
                             pos
                             (CCList.filter (fun x -> x#root != self#root) (React.S.value s_items))

    method private get_parent : Dom_html.element Js.t =
      let par_opt = Js.Opt.to_option self#root##.parentNode |> CCOpt.map Js.Unsafe.coerce in
      CCOpt.get_exn par_opt
    method private get_parent_pos : Position.t =
      let par = self#get_parent in
      { x = par##.offsetLeft
      ; y = par##.offsetTop
      ; w = par##.offsetWidth
      ; h = par##.offsetHeight
      }

    method private ev_to_pos ev : Position.t =
      { x = ev##.clientX;          y = ev##.clientY;
        w = self#get_offset_width; h = self#get_offset_height }

    method private start_dragging ev =
      let init_ev_pos = self#ev_to_pos (ev :> Dom_html.mouseEvent Js.t) in
      (* e##.dataTransfer##setData (Js.string "text/html") (Js.string "anything"); *)
      ghost#style##.zIndex := Js.string "1";
      (* add ghost item to dom to show possible element position *)
      Dom.appendChild self#get_parent ghost#root;
      (* hide element itself while dragging *)
      Dom_html.setTimeout (fun () -> self#style##.opacity := (Js.def @@ Js.string "0")) 0.1 |> ignore;
      Dom_events.listen self#root drag_ev    (fun _ ev -> self#apply_position ~ev ~init_ev_pos; false)
      |> (fun x -> mov_listener <- Some x);
      Dom_events.listen self#root dragend_ev (fun _ ev -> self#apply_position ~ev ~init_ev_pos; false)
      |> (fun x -> end_listener <- Some x)

    method private apply_position ~ev ~init_ev_pos =
      match ev##.clientX,ev##.clientY with
      | 0,0 -> ()
      | _   ->
         match Js.to_string ev##._type with
         | t when t = drag_str ->
            let open Utils in
            let x   = (self#get_offset_left + ev##.clientX - init_ev_pos.x) // (React.S.value s_col_w) in
            let y   = (self#get_offset_top  + ev##.clientY - init_ev_pos.y) // (React.S.value s_row_h) in
            let pos = Position.correct_xy { self#pos with x;y } grid.cols grid.rows in
            if not (self#has_collision pos) then ghost#s_pos_push pos
         | t when t = dragend_str ->
            self#style##.opacity := Js.def @@ Js.string "1";
            CCOpt.iter (fun l -> Dom_events.stop_listen l) mov_listener;
            CCOpt.iter (fun l -> Dom_events.stop_listen l) end_listener;
            (* update element position from ghost *)
            self#s_pos_push ghost#pos;
            Dom.removeChild self#get_parent ghost#root
         | _ -> ()

    method private start_resizing ev =
      ghost#style##.zIndex := Js.string "3";
      Dom.appendChild self#get_parent ghost#root;
      let init_ev_pos = self#ev_to_pos (ev :> Dom_html.mouseEvent Js.t) in
      (* add resize/stop resize event listeners *)
      Dom_events.listen Dom_html.document Dom_events.Typ.mousemove
                        (fun _ ev -> self#apply_size ~ev ~init_ev_pos; false)
      |> (fun x -> mov_listener <- Some x);
      Dom_events.listen Dom_html.document Dom_events.Typ.mouseup
                        (fun _ ev -> self#apply_size ~ev ~init_ev_pos; false)
      |> (fun x -> end_listener <- Some x);
      Dom_html.stopPropagation ev

    method private apply_size ~ev ~init_ev_pos =
      match Js.to_string ev##._type with
      | "mousemove" ->
         let open Utils in
         let col_px = React.S.value s_col_w in
         let row_px = React.S.value s_row_h in
         let w   = init_ev_pos.w + ev##.clientX - init_ev_pos.x in
         let h   = init_ev_pos.h + ev##.clientY - init_ev_pos.y in
         let pos = correct_wh ?max_w:item.max_w
                              ?min_w:item.min_w
                              ?max_h:item.max_h
                              ?min_h:item.min_h
                              { self#pos with w = w // col_px;
                                              h = h // row_px }
                              grid.cols
                              grid.rows
         in
         if not (self#has_collision pos) then ghost#s_pos_push pos;
         (* temporary update element's width and height in pixels *)
         let pos_px = correct_wh ?max_w:(CCOpt.map (fun x -> x * col_px) item.max_w)
                                 ~min_w:(CCOpt.get_or ~default:col_px
                                                      (CCOpt.map (fun x -> x * col_px) item.min_w))
                                 ?max_h:(CCOpt.map (fun x -> x * row_px) item.max_h)
                                 ~min_h:(CCOpt.get_or ~default:row_px
                                                      (CCOpt.map (fun x -> x * row_px) item.min_h))
                                 { x = self#pos.x * col_px
                                 ; y = self#pos.x * col_px
                                 ; w
                                 ; h
                                 }
                                 (grid.cols * col_px)
                                 (CCOpt.map (fun x -> x * row_px) grid.rows)
         in
         self#set_w pos_px.w;
         self#set_h pos_px.h;
      | "mouseup" ->
         CCOpt.iter (fun l -> Dom_events.stop_listen l) mov_listener;
         CCOpt.iter (fun l -> Dom_events.stop_listen l) end_listener;
         (* update element position from ghost *)
         self#s_pos_push ghost#pos;
         Dom.removeChild self#get_parent ghost#root
      | _ -> ()

    initializer
      (* set draggable attribute on element if necessary *)
      (match dragstart_str with
       | "dragstart" -> self#set_attribute "draggable" (string_of_bool item.draggable)
       | _           -> self#set_attribute "draggable" "");
      (* append resize button to element if necessary *)
      if item.resizable then Dom.appendChild self#root resize_button#root;
      (* append widget to cell if provided *)
      CCOpt.iter (fun x -> Dom.appendChild self#root x#root) item.widget;

      (* add item move listener *)
      Dom_events.listen self#root dragstart_ev (fun _ e -> if item.draggable then self#start_dragging e; true)
      |> ignore;

      (* add item start resize listener if needed *)
      Dom_events.listen resize_button#root Dom_events.Typ.mousedown
                        (fun _ e -> if item.resizable then self#start_resizing e; false)
      |> ignore

  end

end

type add_error = Collides  of Position.t list
               | Cancelled
               | In_progress

class t ~grid ~(items:item list) () =
  let e_modify,e_modify_push = React.E.create () in
  let s_col_w,s_col_w_push   = React.S.create grid.min_col_width in
  let s_row_h,_              = match grid.row_height with
    | Some rh -> React.S.create rh
    | None    -> s_col_w, s_col_w_push
  in
  (* let s_row_h,s_row_h_push = React.S.create 0 in *)
  let s_items  = React.S.fold (fun acc x -> match x with
                                            | `Add x    -> x :: acc
                                            | `Remove x -> CCList.filter (fun i -> i#root != x#root) acc)
                             [] e_modify in
  let items    = CCList.map (fun item -> new Item.t ~grid ~s_items ~e_modify_push ~s_col_w ~s_row_h ~item ())
                            items
  in
  let s_change =
    let m a x = x :: a in
    React.S.map (fun l -> React.S.merge m [] (CCList.map (fun x -> x#s_pos) l)) s_items
    |> React.S.switch
  in
  let s_changing =
    let m a x = x :: a in
    React.S.map (fun l -> React.S.merge m [] (CCList.map (fun x -> x#ghost#s_pos) l)) s_items
    |> React.S.switch
  in
  let s_rows = match grid.rows with
    | Some h -> React.S.const h
    | None   -> let merge = (fun acc (x:Position.t) -> if (x.h + x.y) > acc then (x.h + x.y) else acc) in
                React.S.map (fun (l:Position.t list) -> CCList.fold_left merge 0 l) s_changing
  in
  let elt = Markup.Dynamic_grid.create ~items:[] () |> Tyxml_js.To_dom.of_element in

  object(self)

    inherit Widget.widget elt ()

    val mutable adding = false
    val mutable in_dom = false

    (** API **)

    method s_changing = s_changing
    method s_change   = s_change
    method s_items    = s_items

    method items      = React.S.value s_items
    method positions  = React.S.value s_change

    method remove (x:Item.t) = x#remove

    method add (x:item) =
      let items = CCList.map (fun x -> x#pos) (React.S.value s_items) in
      match Position.get_all_collisions ~f:(fun x -> x) x.pos items with
      | [] -> let item = new Item.t ~grid ~e_modify_push ~s_col_w ~s_row_h ~s_items ~item:x () in
              e_modify_push (`Add item);
              Dom.appendChild self#root item#root;
              Ok ()
      | l  -> Error (Collides l)

    method add_free ?min_w ?min_h ?max_w ?max_h
                    ?(static=false)
                    ?(resizable=true)
                    ?(draggable=true)
                    ?widget
                    () =
      if adding
      then Lwt.return_error In_progress
      else
        let open Lwt.Infix in
        adding <- true;
        let t,wakener = Lwt.wait () in
        let item = { pos = Position.empty
                   ; min_w; min_h; max_w; max_h; static; resizable; draggable; widget } in
        let items = CCList.map (fun x -> x#pos) @@ React.S.value s_items in
        let ghost = new Item.cell ~typ:`Ghost ~s_col_w ~s_row_h ~item () in
        Dom.appendChild self#root ghost#root;
        let mv_l  =
          Dom_events.listen Dom_html.document##.body
                            Dom_events.Typ.mousemove
                            (fun _ e -> (match self#get_event_pos e with
                                         | Some ev_pos ->
                                            let ev_pos =
                                              { ev_pos with x = ev_pos.x / React.S.value s_col_w;
                                                            y = ev_pos.y / React.S.value s_row_h
                                              }
                                            in
                                            let pos = Position.get_free_rect ~f:(fun x -> x)
                                                                             ev_pos
                                                                             items
                                                                             grid.cols
                                                                             (React.S.value s_rows)
                                            in
                                            begin match pos with
                                            | Some pos -> ghost#s_pos_push pos
                                            | None     -> ghost#s_pos_push Position.empty
                                            end;
                                         | None -> ghost#s_pos_push Position.empty);
                                        true)
        in
        let cl_l  =
          Dom_events.listen Dom_html.document##.body
                            Dom_events.Typ.click
                            (fun _ _ -> begin match ghost#pos with
                                        | x when x.w = 0 || x.h = 0 -> Lwt.wakeup wakener (Error (Collides []))
                                        | pos -> Lwt.wakeup wakener (self#add { item with pos })
                                        end;
                                        false)
        in
        let esc_l =
          Dom_events.listen Dom_html.document
                            Dom_events.Typ.keydown
                            (fun _ ev -> let key  = CCOpt.map Js.to_string @@ Js.Optdef.to_option ev##.key in
                                         (match key,ev##.keyCode with
                                          | Some "Esc"     ,_
                                            | Some "Escape",_
                                            | _,27 -> Lwt.wakeup wakener (Error (Collides []))
                                          | _      -> ());
                                         true)
        in
        t >>= (fun _ -> adding <- false;
                        Dom_events.stop_listen mv_l;
                        Dom_events.stop_listen cl_l;
                        Dom_events.stop_listen esc_l;
                        Dom.removeChild self#root ghost#root;
                        Lwt.return_unit) |> ignore;
        t

    method layout =
      let w   = self#get_offset_width in
      let col = w / grid.cols in
      let res = w mod col in
      s_col_w_push col;
      self#style##.width := Js.string @@ Printf.sprintf "calc(100%% - %dpx)" res

    (** Private methods *)

    method private get_event_pos e : Position.t option =
      let x = (CCOpt.get_exn @@ Js.Optdef.to_option e##.pageX) - self#get_offset_left in
      let y = (CCOpt.get_exn @@ Js.Optdef.to_option e##.pageY) - self#get_offset_top in
      let (pos:Position.t) = { x; y; w=1;h=1 } in
      if x <= self#get_offset_width && x >= 0 && y<= self#get_offset_height && y >= 0
      then Some pos else None

    initializer
      (* set min/max width of grid *)
      self#style##.minWidth := Utils.px (grid.cols * grid.min_col_width);
      CCOpt.iter (fun x -> self#style##.maxWidth := Utils.px @@ grid.cols * x) grid.max_col_width;
      (* add item add/remove listener *)
      React.E.map (function
                   | `Add (x:Item.t) -> Dom.appendChild self#root x#root
                   | `Remove x       -> Dom.removeChild self#root x#root) e_modify |> ignore;
      (* add initial items *)
      CCList.iter (fun x -> e_modify_push (`Add x)) items;
      (* add height update listener *)
      React.S.l2 (fun h row_h -> self#style##.height := Utils.px (h * row_h)) s_rows s_row_h |> ignore;
      Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ -> self#layout; true) |> ignore;
      Dom_events.listen elt Dom_events.Typ.drop (fun _ ev -> Dom.preventDefault ev; false)   |> ignore;
      MutationObserver.observe
        ~node:Dom_html.document
        ~f:(fun _ _ -> let in_dom_new = (Js.Unsafe.coerce Dom_html.document)##contains self#root in
                       (match in_dom,in_dom_new with
                        | false, true -> self#layout
                        | _           -> ());
                       in_dom <- in_dom_new)
        ~child_list:true
        ~subtree:true
        ()
      |> ignore

end
