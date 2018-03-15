open Containers

include Dynamic_grid_types
include Dynamic_grid_overlay
module Item = Dynamic_grid_item

type add_error = Collides of Position.t list

let to_grid ?max_col_width ?(min_col_width=1) ?rows ?row_height ?(vertical_compact=false)
            ?items_margin ?(multi_select=false) ?(restrict_move=false) ~cols () =
  { min_col_width; max_col_width; cols; rows;
    row_height; vertical_compact; items_margin;
    multi_select; restrict_move
  }

class ['a] t ~grid ~(items:'a item list) () =
  let e_modify,e_modify_push     = React.E.create () in
  let s_selected,s_selected_push = React.S.create ~eq:(fun _ _ -> false) [] in
  let s_col_w,s_col_w_push       = React.S.create grid.min_col_width in
  let s_row_h = match grid.row_height with
    | Some rh -> React.S.const rh
    | None    -> s_col_w
  in
  let s_item_margin,s_item_margin_push = React.S.create (Option.get_or ~default:(0,0) grid.items_margin) in
  let s_items  = React.S.fold (fun acc -> function
                                | `Add x    -> x :: acc
                                | `Remove x -> List.filter Fun.(Item.eq x %> not) acc)
                              [] e_modify
  in
  let new_item item = new Item.t ~grid ~s_items ~e_modify_push ~s_selected ~s_selected_push
                          ~s_col_w ~s_row_h ~s_item_margin ~item ()
  in
  let items      = List.map (fun item -> new_item item) items in
  let s_change   = let m a x = x :: a in
                   React.S.map (fun l -> React.S.merge m [] (List.map (fun x -> x#s_change) l)) s_items
                   |> React.S.switch
  in
  let s_changing = let m a x = x :: a in
                   React.S.map (fun l -> React.S.merge m [] (List.map (fun x -> x#s_changing) l)) s_items
                   |> React.S.switch
  in
  let s_rows     = match grid.rows with
    | Some h -> React.S.const h
    | None   -> let merge = (fun acc (x:Position.t) -> if (x.h + x.y) > acc then (x.h + x.y) else acc) in
                React.S.map (fun (l:Position.t list) -> List.fold_left merge 1 l) s_changing
  in
  let elt = Markup.Dynamic_grid.create ~items:[] () |> Tyxml_js.To_dom.of_element in

  object(self)

    inherit Widget.widget elt ()

    val overlay_grid      = new overlay_grid ~parent:elt ()
    val _s_selected       = React.S.map (fun x -> x) s_selected
    val _e_selected       = React.S.changes s_selected

    (** API **)

    method s_changing = s_changing
    method s_change   = s_change
    method s_items    = s_items

    method s_selected : 'a Item.t list React.signal = _s_selected
    method e_selected : 'a Item.t list React.event  = _e_selected

    method items      = Position.sort_by_y ~f:(fun x -> x#pos) @@ React.S.value s_items
    method positions  = React.S.value s_change

    method get_item_margin = React.S.value s_item_margin
    method set_item_margin margin = s_item_margin_push margin

    method overlay_grid = overlay_grid

    method add (x:'a item) =
      let items = List.map (fun x -> x#pos) (React.S.value s_items) in
      match Position.get_all_collisions ~f:(fun x -> x) x.pos items with
      | [] -> let item = new_item x in
              e_modify_push (`Add item);
              Dom.appendChild self#root item#root;
              Ok item
      | l  -> Error (Collides l)

    method remove (x:'a Item.t) = x#remove
    method remove_all = List.iter (fun x -> self#remove x) self#items

    method layout =
      let par = Js.Opt.to_option @@ self#root##.parentNode in
      (match par with
       | Some p -> let w  = (Js.Unsafe.coerce p)##.offsetWidth in
                   Printf.printf "offset widht: %d\n" w;
                   let mx,_ = React.S.value s_item_margin in
                   let w    = if w < grid.cols then grid.cols else w in
                   let col  = w / grid.cols in
                   s_col_w_push col;
                   self#style##.width := Js.string @@ Printf.sprintf "%dpx" (col * grid.cols + mx);
                   overlay_grid#layout col (React.S.value s_row_h) (React.S.value s_item_margin)
       | None   -> ())

    (** Private methods **)

    method private s_item_margin = s_item_margin
    method private s_col_w       = s_col_w
    method private s_row_h       = s_row_h
    method private s_rows        = s_rows

    method private compact =
      let other i = List.filter (fun x -> not @@ Equal.physical x#root i#root) self#items in
      List.iter (fun x -> x#set_pos @@ Position.compact ~f:(fun x -> x#pos) x#pos (other x)) self#items

    initializer
      (* add item add/remove listener *)
      React.E.map (fun action -> (match action with
                                  | `Add (x:'a Item.t) -> Dom.appendChild self#root x#root
                                  | `Remove x          -> Dom.removeChild self#root x#root);
                                 (* FIXME make vertical compact variable *)
                                 if grid.vertical_compact then self#compact) e_modify
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
      Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ -> self#layout; true) |> ignore;

  end
