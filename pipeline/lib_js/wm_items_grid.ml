open Containers
open Components
open Wm_types
open Wm_components

let base_class = "wm-grid"

module Layer(I : Item) = struct

  let layout_pos_of_grid_pos ~resolution ~cols ~rows (pos:Dynamic_grid.Position.t) : Wm.position =
    let w,h    = resolution in
    let cw,rh  = w / cols, h / rows in
    { left   = pos.x * cw
    ; right  = (pos.w + pos.x) * cw
    ; top    = pos.y * rh
    ; bottom = (pos.h + pos.y) * rh
    }

  let grid_pos_of_layout_pos ~resolution ~cols ~rows (pos:Wm.position) : Dynamic_grid.Position.t =
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
    Dynamic_grid.Item.to_item ~pos ~value:x ()

  class t ~layer ~resolution ~cols ~rows ~init () =
    let _class  = Markup.CSS.add_element base_class "grid" in
    let ph      = Placeholder.make ~text:"Добавьте элементы в раскладку" ~icon:"add_box" () in
    let grid    = Dynamic_grid.to_grid ~cols ~rows ~items_margin:(2,2) () in
    let items   = List.map (item_to_grid_item ~resolution ~cols ~rows) init in
    object(self)

      inherit [I.t] Dynamic_grid.t ~grid ~items ()

      val mutable typ          = ""
      val mutable layer        = layer
      val mutable enter_target = Js.null

      method layer : int = layer
      method set_layer x = layer <- x;
                           List.iter (fun x -> x#set_value (I.update_layer x#get_value layer)) self#items

      method private update_item_value item position =
        let pos = layout_pos_of_grid_pos ~resolution ~cols ~rows position in
        item#set_value (I.update_position item#get_value pos)

      method private add_from_candidate pos ac =
        let open Dynamic_grid in
        if not @@ Position.equal pos Position.empty
        then
          let other    = List.map (fun x -> x#get_value) self#items in
          let (ac:I.t) = if not (ac:I.t).unique
                         then { ac with name = I.make_item_name ac other }
                         else ac
          in
          let item  = Dynamic_grid.Item.to_item ~pos ~value:ac () in
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
                                       Result.iter (self#add_from_candidate ghost#pos)
                                                   (I.of_yojson json);
                                       ghost#set_pos Dynamic_grid.Position.empty;
                                       true)
         |> ignore)

    end

end

module Make(I : Item) = struct

  module G = Layer(I)

  class t ~title ~resolution ~cols ~rows ~(init: I.t list) ~e_layers ~s_conf () =
    let grouped =
      List.fold_left (fun acc (x:I.t) ->
          let layer = I.layer_of_t x in
          List.Assoc.update ~eq:(=) ~f:(function Some l -> Some (x :: l) | None -> Some [x]) layer acc)
                     [] init
    in
    let layers  = I.layers_of_t_list init in
    let s_grids,s_grids_push =
      List.map (fun x -> let items = List.Assoc.get ~eq:(=) x grouped in
                         let init  = Option.get_or ~default:[] items in
                         new G.t ~layer:x ~init ~cols ~rows ~resolution ()) layers
      |> React.S.create
    in
    let s_active_grid,s_active_grid_push = React.S.create @@ List.hd @@ React.S.value s_grids in
    let wrapper = Dom_html.createDiv Dom_html.document |> Widget.create in
    let title   = new Typography.Text.t ~font:Subheading_2 ~text:title () in
    object(self)

      inherit Box.t ~vertical:true ~widgets:[title#widget;wrapper#widget] ()

      val s_sel =
        let eq = fun _ _ -> false in
        React.S.map ~eq:Equal.physical
                    (fun x -> let s = React.S.hold ~eq [] x#e_selected in
                              React.S.map ~eq (function [x] -> Some x | _ -> None) s)
                    s_active_grid
        |> React.S.switch ~eq

      method s_selected   = s_sel
      method layout_items = List.map I.t_to_layout_item self#items
      method items        = List.fold_left (fun acc x -> x#items @ acc) [] @@ React.S.value s_grids
                            |> List.map (fun x -> x#get_value)

      initializer
        React.S.l2 (fun conf grid -> if conf.show_grid_lines then grid#overlay_grid#show
                                     else grid#overlay_grid#hide)
                   s_conf s_active_grid |> ignore;
        React.S.map (fun grid -> Utils.rm_children wrapper#root;
                                 Dom.appendChild wrapper#root grid#root) s_active_grid |> ignore;
        React.E.map (fun e -> let grids = React.S.value s_grids in
                              match e with
                              | `Selected x    ->
                                 let grid = List.find_pred (fun g -> g#layer = x) grids in
                                 Option.iter (fun g -> s_active_grid_push g) grid
                              | `Added x       ->
                                 let grid = new G.t ~layer:x ~init:[] ~cols ~rows ~resolution () in
                                 s_grids_push (grid :: grids)
                              | `Removed x     ->
                                 s_grids_push @@ List.filter (fun g -> g#layer <> x) grids
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
           ~s_conf
           () =
    let positions = List.map I.position_of_t init in
    let cols,rows = Utils.get_grid ~resolution ~positions () in
    let ig = new t ~title ~resolution ~cols ~rows ~init ~e_layers ~s_conf () in
    let _  = React.S.map (fun x -> selected_push x) ig#s_selected in
    ig

end