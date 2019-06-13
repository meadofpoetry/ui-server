open Js_of_ocaml
open Components
open Wm_types
open Basic_widgets
open Dynamic_grid
open Pipeline_types

let base_class = "wm-grid"

let layout_pos_of_grid_pos ~resolution
    ~cols ~rows (pos : Position.t) : Wm.position =
  let w, h = resolution in
  let cw, rh = w / cols, h / rows in
  { left = pos.x * cw
  ; right = (pos.w + pos.x) * cw
  ; top = pos.y * rh
  ; bottom = (pos.h + pos.y) * rh
  }

let grid_pos_of_layout_pos ~resolution
    ~cols ~rows (pos : Wm.position) : Position.t =
  let w, h = resolution in
  let cw, rh = w / cols, h / rows in
  { x = pos.left / cw
  ; y = pos.top / rh
  ; w = (pos.right - pos.left) / cw
  ; h = (pos.bottom - pos.top) / rh
  }

module Make(I : Item) = struct

  module Position = Dynamic_grid.Position

  class t ~layer ~resolution ~s_grid ~init () =
    let c, r = React.S.value s_grid in
    let e_dblclick, e_dblclick_push = React.E.create () in
    let e_delete, e_delete_push = React.E.create () in
    let _class = Components_tyxml.BEM.add_element base_class "grid" in
    let ph =
      Placeholder.make
        ~text:"Добавьте элементы в раскладку"
        ~icon:Icon.SVG.(make_simple Path.plus_box) () in
    let grid = Dynamic_grid.to_grid
        ~cols:c ~rows:r
        ~restrict_move:true
        ~items_margin:(2, 2)
        () in
    object(self)

      inherit [I.t] Dynamic_grid.t ~grid ~items:[] () as super

      val mutable typ = ""
      val mutable layer = layer
      val mutable enter_target = Js.null

      (* API *)

      method! init () : unit =
        super#init ();
        self#add_class _class;
        self#set_active false;
        self#set_layer layer;
        (* self#set_on_load @@ Some self#layout; *)
        React.S.map self#set_grid s_grid |> ignore;
        React.S.map (function
            | [] -> self#append_child ph
            | _-> self#remove_child ph)
          self#s_items |> ignore;
        List.iter self#add_from_candidate init;
        List.iter (fun i -> React.S.map (fun p -> self#update_item_value i p)
                              i#s_change |> ignore)
          self#items;
        let ghost =
          new Dynamic_grid.Item.cell
            ~typ:`Ghost
            ~s_grid:self#s_grid
            ~s_col_w:self#s_col_w
            ~s_row_h:self#s_row_h
            ~pos:Dynamic_grid.Position.empty
            () in
        ghost#root##.style##.zIndex := Js.string "10000";
        self#append_child ghost;
        Lwt.async (fun () ->
            Events.dragenters super#root (fun e _ ->
                Dom_html.stopPropagation e;
                Dom.preventDefault e;
                enter_target <- e##.target;
                Lwt.return_unit));
        Lwt.async (fun () ->
            Events.dragleaves super#root (fun e _ ->
                Dom_html.stopPropagation e;
                Dom.preventDefault e;
                if enter_target == e##.target
                then ghost#set_pos Dynamic_grid.Position.empty;
                Lwt.return_unit));
        Lwt.async (fun () ->
            Events.dragovers super#root (fun e _ ->
                let a = Js.Unsafe.coerce e##.dataTransfer##.types in
                let l = Js.to_array a |> Array.to_list |> List.map Js.to_string in
                let t = Utils.List.find_map (fun x ->
                    match Utils.String.chop_prefix ~pre:List_of_items.drag_type_prefix x with
                    | Some wh -> typ <- x; Some wh
                    | None    -> None) l in
                Utils.Option.iter (fun wh ->
                    let aspect = match String.split_on_char ':' wh with
                      | w :: h :: [] ->
                        begin match int_of_string_opt w, int_of_string_opt h with
                          | Some w, Some h -> Some (w, h)
                          | _ -> None
                        end
                      | _ -> None
                    in
                    let p = self#get_event_pos (e :> Dom_html.mouseEvent Js.t) in
                    match p with
                    | Some _ -> self#move_ghost ?aspect ghost p;
                      let gp = ghost#pos in
                      if not @@ Dynamic_grid.Position.(equal gp empty)
                      then Dom.preventDefault e;
                    | None -> ()) t;
                Lwt.return_unit));
        Lwt.async (fun () ->
            Events.drops super#root (fun e _ ->
                Dom.preventDefault e;
                let json = e##.dataTransfer##getData (Js.string typ)
                           |> Js.to_string
                           |> Yojson.Safe.from_string in
                (match I.of_yojson json with
                 | Error _ -> ()
                 | Ok t ->
                   let open Dynamic_grid.Position in
                   let pos = ghost#pos in
                   if not @@ equal pos empty
                   then
                     let cols, rows = React.S.value s_grid in
                     let pos = layout_pos_of_grid_pos
                         ~resolution ~cols ~rows pos in
                     let other = List.map (fun x -> x#value) self#items in
                     let (t : I.t) =
                       if not (t:I.t).unique
                       then { t with name = I.make_item_name t other }
                       else t in
                     let (t : I.t) = I.update_position t pos in
                     self#add_from_candidate t);
                ghost#set_pos Dynamic_grid.Position.empty;
                Lwt.return_unit))

      method set_active x =
        super#toggle_class ~force:(not x)
        @@ Components_tyxml.BEM.add_modifier _class "background"

      method set_visible x =
        super#toggle_class ~force:(not x)
        @@ Components_tyxml.BEM.add_modifier _class "invisible"

      method layer : int =
        layer

      method set_layer x =
        layer <- x;
        self#set_attribute "data-layer" @@ string_of_int layer;
        List.iter (fun x -> x#set_value (I.update_layer x#value layer)) self#items

      method e_item_dblclick = e_dblclick

      method e_item_delete = e_delete

      (* Private methods *)

      method private move_ghost ?aspect ghost = function
        | None ->
          ghost#set_pos Position.empty
        | Some epos ->
          let open Position in
          let epos = { epos with x = epos.x / React.S.value self#s_col_w;
                                 y = epos.y / React.S.value self#s_row_h } in
          let items = List.map (fun x -> x#pos) self#items in
          let pos =
            get_free_rect ?aspect ~f:(fun x -> x) epos items self#grid.cols
              (React.S.value self#s_rows) () in
          (match pos with
           | Some x -> ghost#set_pos x
           | None -> ghost#set_pos empty)

      method private update_item_value item position =
        let cols,rows = React.S.value s_grid in
        let pos = layout_pos_of_grid_pos ~resolution ~cols ~rows position in
        item#set_value (I.update_position item#value pos)

      method private add_from_candidate t =
        let pos = I.position_of_t t in
        let cols, rows = React.S.value s_grid in
        let pos = grid_pos_of_layout_pos ~resolution ~cols ~rows pos in
        let (t : I.t) = I.update_layer t self#layer in
        let item = I.to_grid_item t pos in
        match self#add item with
        | Error _ -> ()
        | Ok (i : #Widget.t) ->
          ignore @@ React.S.map (fun p -> self#update_item_value i p) i#s_change;
          Lwt.async (fun () ->
              Events.dblclicks i#root (fun _ _ ->
                  e_dblclick_push i;
                  Lwt.return_unit));
          Lwt.async (fun () ->
              Events.keydowns i#root (fun e _ ->
                  match Components.Events.Key.of_event e with
                  | `Delete -> e_delete_push i; Lwt.return_unit
                  | _ -> Lwt.return_unit))

      method private set_grid ((cols, rows) : int * int) =
        List.iter (fun i ->
            let pos = I.position_of_t i#value in
            let pos = grid_pos_of_layout_pos ~resolution ~cols ~rows pos in
            i#set_pos pos) self#items;
        self#s_grid_push { self#grid with cols; rows = Some rows };
        self#layout ()

    end

end
