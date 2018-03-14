open Containers
open Components
open Wm_types
open Wm_components

let base_class = "wm-grid"

module Layer(I : Item) = struct

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

      method private move_ghost ?width ?height ghost = function
        | None      -> ghost#set_pos Position.empty
        | Some epos -> let epos =
                         Position.({ epos with x = epos.x / React.S.value self#s_col_w;
                                               y = epos.y / React.S.value self#s_row_h })
                       in
                       let items = List.map (fun x -> x#pos) self#items in
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
                                 let width,height = match String.split_on_char ':' wh with
                                   | w :: h :: [] -> Int.of_string w, Int.of_string h
                                   | _            -> None,None
                                 in
                                 let p = self#get_event_pos (e :> Dom_html.mouseEvent Js.t) in
                                 match p with
                                 | Some _ -> self#move_ghost ?width ?height ghost p;
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

module Make(I : Item) = struct

  module G = Layer(I)

  class t ~title ~resolution ~(init: I.t list) ~e_layers () =
    let positions         = List.map I.position_of_t init in
    let s_grids,set_grids = React.S.create @@ Utils.get_grids ~resolution ~positions () in
    let s_grid,set_grid   = React.S.create @@ Utils.get_best_grid ~cols:90 ~resolution (React.S.value s_grids) in
    let (cols,rows)       = React.S.value s_grid in
    let grouped =
      List.fold_left (fun acc (x:I.t) ->
          let layer = I.layer_of_t x in
          List.Assoc.update ~eq:(=) ~f:(function Some l -> Some (x :: l) | None -> Some [x]) layer acc)
                     [] init
    in
    let layers  = I.layers_of_t_list init in
    let s_layers,set_layers =
      List.map (fun x -> let items = List.Assoc.get ~eq:(=) x grouped in
                         let init  = Option.get_or ~default:[] items in
                         new G.t ~layer:x ~init ~cols ~rows ~resolution ()) layers
      |> React.S.create
    in
    let s_active,set_active = React.S.create @@ List.hd @@ React.S.value s_layers in
    let wrapper    = Dom_html.createDiv Dom_html.document |> Widget.create in
    let title      = new Typography.Text.t ~adjust_margin:false ~text:title () in
    let menu_icon  = new Icon.Button.Font.t ~icon:"more_horiz" () in
    let on_data    = ({ icon = "grid_off"; label = None; css_class = None }:Markup.Icon_toggle.data) in
    let off_data   = ({ icon = "grid_on"; label = None; css_class = None }:Markup.Icon_toggle.data) in
    let grid_icon  = new Icon_toggle.t ~on_data ~off_data () in
    let menu_items = List.map (fun (w,h) -> `Item (new Menu.Item.t ~text:(Printf.sprintf "%dx%d" w h) ()))
                     @@ React.S.value s_grids in
    let menu       = new Menu.t ~items:menu_items () in
    let menu_wrap  = new Menu.Wrapper.t ~anchor:menu_icon ~menu () in
    let icons      = new Box.t ~vertical:false ~widgets:[ grid_icon#widget; menu_wrap#widget ] () in
    let header     = new Box.t ~vertical:false ~widgets:[ title#widget;icons#widget ] () in
    object(self)

      inherit Box.t ~vertical:true ~widgets:[header#widget;wrapper#widget] ()

      val s_sel =
        let eq = fun _ _ -> false in
        React.S.map ~eq:Equal.physical
                    (fun x -> let s = React.S.hold ~eq [] x#e_selected in
                              React.S.map ~eq (function [x] -> Some x | _ -> None) s)
                    s_active
        |> React.S.switch ~eq

      method s_layers     = s_layers
      method s_selected   = s_sel
      method layout_items = List.map I.t_to_layout_item self#items
      method items        = List.fold_left (fun acc x -> x#items @ acc) [] @@ React.S.value s_layers
                            |> List.map (fun x -> x#get_value)

      initializer
        grid_icon#set_on true;
        (* update available grids *)
        let eq = (fun _ _ -> false) in
        let s = React.S.switch ~eq (React.S.map ~eq (fun x -> x#s_change) s_active) in
        React.S.map ~eq (fun _ -> let positions = List.map (fun x -> I.position_of_t x) self#items in
                                  let grids = Utils.get_grids ~resolution ~positions () in
                                  set_grids grids) s |> ignore;
        (* update selected grid *)
        React.E.map (fun (_,i) -> let s = i##.textContent
                                          |> Js.Opt.to_option
                                          |> Option.get_exn
                                          |> Js.to_string
                                  in
                                  match String.split_on_char 'x' s with
                                  | w::h::[] -> (match Int.of_string w, Int.of_string h with
                                                 | Some w,Some h -> set_grid (w,h)
                                                 | _             -> ())
                                  | _ -> ()) menu#e_selected |> ignore;
        (* set new grid *)
        React.S.map (fun (w,h) ->
            let grids = React.S.value s_layers in
            let grids = List.map (fun g -> let init = List.map (fun x -> x#get_value) g#items in
                                           new G.t ~layer:g#layer ~init ~cols:w ~rows:h ~resolution ())
                                 grids
            in
            let ag = React.S.value s_active in
            let ag = List.find (fun x -> x#layer = ag#layer) grids in
            set_layers grids;
            set_active ag;
            grids) s_grid |> ignore;
        React.E.map (fun _ -> let item text = new Menu.Item.t ~text () in
                              let items = List.map (fun (w,h) -> let text = Printf.sprintf "%dx%d" w h in
                                                                 item text) @@ React.S.value s_grids in
                              Utils.rm_children menu#get_list#root;
                              List.iter (fun x -> Dom.appendChild menu#get_list#root x#root) items;
                              menu#show) menu_icon#e_click |> ignore;
        title#add_class     @@ Markup.CSS.add_element base_class "title";
        menu_icon#add_class @@ Markup.CSS.add_element base_class "menu";
        grid_icon#add_class @@ Markup.CSS.add_element base_class "menu";
        header#add_class    @@ Markup.CSS.add_element base_class "header";
        React.S.l2 (fun conf grid -> if conf then grid#overlay_grid#show else grid#overlay_grid#hide)
                   grid_icon#s_state s_active |> ignore;
        React.S.map (fun grid -> Utils.rm_children wrapper#root;
                                 Dom.appendChild wrapper#root grid#root) s_active |> ignore;
        React.E.map (fun e -> let grids = React.S.value s_layers in
                              match e with
                              | `Selected x    ->
                                 let grid = List.find_pred (fun g -> g#layer = x) grids in
                                 Option.iter (fun g -> set_active g) grid
                              | `Added x       ->
                                 let grid = new G.t ~layer:x ~init:[] ~cols ~rows ~resolution () in
                                 set_layers (grid :: grids)
                              | `Removed x     ->
                                 set_layers @@ List.filter (fun g -> g#layer <> x) grids
                              | `Changed l ->
                                 List.iter (fun g -> match List.Assoc.get ~eq:(=) g#layer l with
                                                     | Some n -> g#set_layer n
                                                     | None   -> ()) grids)
                    e_layers |> ignore;
        self#add_class base_class;
        self#set_justify_content `Center;
        wrapper#add_class @@ Markup.CSS.add_element base_class "wrapper"

    end

  let make ~title
           ~resolution
           ~(init: I.t list)
           ~selected_push
           ~e_layers
           () =
    let ig = new t ~title ~resolution ~init ~e_layers () in
    let _  = React.S.map (fun x -> selected_push x) ig#s_selected in
    ig

end
