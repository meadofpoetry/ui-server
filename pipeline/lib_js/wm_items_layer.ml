open Containers
open Components
open Wm_types
open Wm_components

let base_class = "wm-grid"

module Make(I : Item) = struct

  module Position = Dynamic_grid.Position

  let layout_pos_of_grid_pos ~resolution ~cols ~rows (pos:Position.t) : Wm.position =
    let w,h    = resolution in
    let cw,rh  = w / cols, h / rows in
    { left   = pos.x * cw
    ; right  = (pos.w + pos.x) * cw
    ; top    = pos.y * rh
    ; bottom = (pos.h + pos.y) * rh
    }

  let grid_pos_of_layout_pos ~resolution ~cols ~rows (pos:Wm.position) : Position.t =
    let w,h   = resolution in
    let cw,rh = w / cols, h / rows in
    { x = pos.left / cw
    ; y = pos.top / rh
    ; w = (pos.right - pos.left) / cw
    ; h = (pos.bottom - pos.top) / rh
    }

  let item_to_grid_item ~resolution ~cols ~rows (x:I.t) =
    let pos = I.position_of_t x in
    let pos = grid_pos_of_layout_pos ~resolution ~cols ~rows pos in
    I.to_grid_item x pos

  class t ~layer ~resolution ~cols ~rows ~init () =
    let _class  = Markup.CSS.add_element base_class "grid" in
    let ph      = Placeholder.make ~text:"Добавьте элементы в раскладку" ~icon:"add_box" () in
    let grid    = Dynamic_grid.to_grid ~cols ~rows ~restrict_move:true ~items_margin:(2,2) () in
    let items   = List.map (item_to_grid_item ~resolution ~cols ~rows) init in
    object(self)

      inherit [I.t] Dynamic_grid.t ~grid ~items ()

      val mutable typ          = ""
      val mutable layer        = layer
      val mutable enter_target = Js.null

      method layer : int = layer
      method set_layer x = layer <- x;
                           List.iter (fun x -> x#set_value (I.update_layer x#get_value layer)) self#items

      method private get_event_pos e : Position.t option =
        let rect = self#get_client_rect in
        let x,y  = e##.clientX - (int_of_float rect.left),
                   e##.clientY - (int_of_float rect.top) in
        if x <= self#get_offset_width && x >= 0 && y <= self#get_offset_height && y >= 0
        then Some { x; y; w = 1; h = 1 } else None

      method private move_ghost ?aspect ?width ?height ghost = function
        | None      -> ghost#set_pos Position.empty
        | Some epos -> let open Position in
                       let epos = { epos with x = epos.x / React.S.value self#s_col_w;
                                              y = epos.y / React.S.value self#s_row_h }
                       in
                       let items = List.map (fun x -> x#pos) self#items in
                       let cmp =
                         match width, height with
                         | Some w, Some h ->
                            Some (fun n o -> if n.w < w || n.h < h || (n.w * n.h) < (o.w * o.h)
                                             then 0 else 1)
                         | Some w, _      -> Some (fun n o -> if n.w < w || n.h < o.h then 0 else 1)
                         | _, Some h      -> Some (fun n o -> if n.h < h || n.w < o.w then 0 else 1)
                         | _              -> None
                       in
                       let pos = get_free_rect ?cmp ?aspect ~f:(fun x -> x) epos items grid.cols
                                               (React.S.value self#s_rows) () in
                       let pos = Option.map (fun pos ->
                                     let corr_x = fun w -> if epos.x + w > pos.x + pos.w
                                                           then (pos.x + pos.w) - w
                                                           else epos.x
                                     in
                                     let corr_y = fun h -> if epos.y + h > pos.y + pos.h
                                                           then (pos.y + pos.h) - h
                                                           else epos.y
                                     in
                                     let pos = match width, height with
                                       | Some w, Some h ->
                                          let w = if w < 1 then 1 else w in
                                          let h = if h < 1 then 1 else h in
                                          { x = corr_x w; y = corr_y h; w; h}
                                       | Some w, _  -> let w = if w < 1 then 1 else w in
                                                       {pos with x = corr_x w; w}
                                       | _, Some h  -> let h = if h < 1 then 1 else h in
                                                       {pos with y = corr_y h; h}
                                       | _          -> pos
                                     in pos) pos
                       in
                       (match pos with
                        | Some x -> ghost#set_pos x
                        | None   -> ghost#set_pos empty)

      method private update_item_value item position =
        let pos = layout_pos_of_grid_pos ~resolution ~cols ~rows position in
        item#set_value (I.update_position item#get_value pos)

      method private add_from_candidate pos ac =
        let open Dynamic_grid in
        if not @@ Position.equal pos Position.empty
        then
          let other    = List.map (fun x -> x#get_value) self#items in
          let (ac:I.t) = I.update_layer ac self#layer in
          let (ac:I.t) = I.update_position ac @@ layout_pos_of_grid_pos ~resolution ~cols ~rows pos in
          let (ac:I.t) = if not (ac:I.t).unique
                         then { ac with name = I.make_item_name ac other }
                         else ac
          in
          let item = I.to_grid_item ac pos in
          Result.iter (fun i -> React.S.map (fun p -> self#update_item_value i p) i#s_change |> ignore)
                      (self#add item)

      initializer
        React.S.map (function
                     | [] -> Dom.appendChild self#root ph#root
                     | _  -> try Dom.removeChild self#root ph#root with _ -> ())
                    self#s_items |> ignore;
        self#set_on_load @@ Some (fun () -> self#layout);
        self#add_class _class;
        List.iter (fun i -> React.S.map (fun p -> self#update_item_value i p) i#s_change |> ignore) self#items;
        (let ghost = new Dynamic_grid.Item.cell
                         ~typ:`Ghost
                         ~s_col_w:self#s_col_w
                         ~s_row_h:self#s_row_h
                         ~s_item_margin:self#s_item_margin
                         ~pos:Dynamic_grid.Position.empty
                         ()
         in
         ghost#style##.zIndex := Js.string "10000";
         Dom.appendChild self#root ghost#root;
         Dom_events.listen self#root Dom_events.Typ.dragenter
                           (fun _ e -> Dom_html.stopPropagation e; Dom.preventDefault e;
                                       enter_target <- e##.target;
                                       true) |> ignore;
         Dom_events.listen self#root Dom_events.Typ.dragleave
                           (fun _ e -> Dom_html.stopPropagation e;Dom.preventDefault e;
                                       if Equal.physical enter_target e##.target
                                       then ghost#set_pos Dynamic_grid.Position.empty;
                                       true) |> ignore;
         Dom_events.listen self#root Dom_events.Typ.dragover
                           (fun _ e ->
                             let a = Js.Unsafe.coerce e##.dataTransfer##.types in
                             let l = Js.to_array a |> Array.to_list |> List.map Js.to_string in
                             let t = List.find_map (fun x ->
                                         match String.chop_prefix ~pre:Wm_items.drag_type_prefix x with
                                         | Some wh -> typ <- x; Some wh
                                         | None    -> None) l in
                             Option.iter (fun wh ->
                                 let aspect = match String.split_on_char ':' wh with
                                   | w :: h :: [] -> (match Int.of_string w, Int.of_string h with
                                                      | Some w, Some h -> Some (w,h)
                                                      | _              -> None)
                                   | _            -> None
                                 in
                                 let p = self#get_event_pos (e :> Dom_html.mouseEvent Js.t) in
                                 match p with
                                 | Some _ -> self#move_ghost ?aspect ghost p;
                                             let gp = ghost#pos in
                                             if not @@ Dynamic_grid.Position.(equal gp empty)
                                             then Dom.preventDefault e;
                                 | None -> ()) t;
                             true)
         |> ignore;
         Dom_events.listen self#root Dom_events.Typ.drop
                           (fun _ e -> Dom.preventDefault e;
                                       let json = e##.dataTransfer##getData (Js.string typ)
                                                  |> Js.to_string
                                                  |> Yojson.Safe.from_string
                                       in
                                       Result.iter (self#add_from_candidate ghost#pos) (I.of_yojson json);
                                       ghost#set_pos Dynamic_grid.Position.empty;
                                       true)
         |> ignore)

    end

end
