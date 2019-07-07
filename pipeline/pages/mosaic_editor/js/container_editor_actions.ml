open Js_of_ocaml
open Components
open Resizable_grid_utils

(* TODO
   [ ] Add cell split action
   [X] Add container info action
   [X] Add container edit action *)

type t =
  { callback : Dom_html.element Js.t list -> unit Lwt.t(* handle selected cells, if any *)
  ; name : string
  ; icon : string
  }

let ( >>= ) = Lwt.bind

let apply_opt_cb = function
  | None -> ()
  | Some f -> f ()

let wizard (grid : Resizable_grid.t) =
  { callback = (fun _ -> Lwt.return_unit)
  ; name = "Мастер"
  ; icon = Icon.SVG.Path.auto_fix
  }

let description ((input : string Textfield.t), (dialog : Dialog.t)) =
  { callback = (function
        | [cell] ->
          let title = get_cell_title cell in
          input#set_value title;
          (dialog#open_await ()
           >>= function
           | Close | Destroy | Custom _ -> Lwt.return_unit
           | Accept ->
             (* TODO
                - Check if title is valid (empty, repeating, etc)
                - Change top app bar title *)
             let value' = input#value_as_string in
             if not (String.equal title value')
             then set_cell_title cell value';
             Lwt.return_unit)
        | _ -> Lwt.return_unit)
  ; name = "Описание"
  ; icon = Icon.SVG.Path.information
  }

let edit f =
  { callback = (function
        | [cell] -> f cell
        | _ -> Lwt.return_unit)
  ; name = "Редактировать"
  ; icon = Icon.SVG.Path.pencil
  }

let merge ?f (grid : Resizable_grid.t) =
  { callback = (fun cells ->
        grid#merge cells;
        apply_opt_cb f;
        Lwt.return_unit)
  ; name = "Объединить ячейки"
  ; icon = Icon.SVG.Path.table_merge_cells
  }

let add_row_above (grid : Resizable_grid.t) =
  { callback = (function
        | [] -> Lwt.return_unit
        | cells ->
          grid#add_row_before
          @@ get_topmost_cell cells;
          Lwt.return_unit)
  ; name = "Добавить ряд сверху"
  ; icon = Icon.SVG.Path.table_row_plus_before
  }

let add_row_below (grid : Resizable_grid.t) =
  { callback = (function
        | [] -> Lwt.return_unit
        | cells ->
          grid#add_row_after
          @@ get_bottommost_cell cells;
          Lwt.return_unit)
  ; name = "Добавить ряд снизу"
  ; icon = Icon.SVG.Path.table_row_plus_after
  }

let remove_row ?f (grid : Resizable_grid.t) =
  { callback = (fun cells ->
        let _, cells =
          List.split
          @@ List.fold_left (fun acc x ->
              let pos = get_cell_position x in
              if List.mem_assoc pos.row acc
              then acc else (pos.row, x) :: acc)
            [] cells in
        List.iter grid#remove_row cells;
        apply_opt_cb f;
        Lwt.return_unit)
  ; name = "Удалить ряд"
  ; icon = Icon.SVG.Path.table_row_remove
  }

let add_col_left (grid : Resizable_grid.t) =
  { callback = (function
        | [] -> Lwt.return_unit
        | cells ->
          grid#add_column_before @@ get_leftmost_cell cells;
          Lwt.return_unit)
  ; name = "Добавить столбец слева"
  ; icon = Icon.SVG.Path.table_column_plus_before
  }

let add_col_right (grid : Resizable_grid.t) =
  { callback = (function
        | [] -> Lwt.return_unit
        | cells ->
          grid#add_column_after @@ get_rightmost_cell cells;
          Lwt.return_unit)
  ; name = "Добавить столбец справа"
  ; icon = Icon.SVG.Path.table_column_plus_after
  }

let remove_col ?f (grid : Resizable_grid.t) =
  { callback = (fun cells ->
        let _, cells =
          List.split
          @@ List.fold_left (fun acc x ->
              let pos = get_cell_position x in
              if List.mem_assoc pos.col acc
              then acc else (pos.col, x) :: acc)
            [] cells in
        List.iter grid#remove_column cells;
        apply_opt_cb f;
        Lwt.return_unit)
  ; name = "Удалить столбец"
  ; icon = Icon.SVG.Path.table_column_remove
  }

let make_icon_button get_selected action =
  let icon =
    Icon_button.make
      ~on_click:(fun _ _ -> action.callback @@ get_selected ())
      ~icon:(Icon.SVG.make_simple action.icon)#root
      () in
  icon#set_attribute "title" action.name;
  icon

let make_menu_item action =
  Item_list.Item.make
    ~role:"menuitem"
    ~graphic:Icon.SVG.(make_simple action.icon) (* FIXME *)
    action.name

let make_overflow_menu
    (get_selected : unit -> Dom_html.element Js.t list)
    (scaffold : Scaffold.t)
    (actions : t list) =
  let actions, menu_items =
    List.split
    @@ List.map (fun x ->
        make_icon_button get_selected x,
        make_menu_item x)
      actions in
  let menu = Menu.make_of_item_list
      ~body:scaffold#app_content_inner
      ~viewport:(Element scaffold#app_content_inner)
      (Item_list.make menu_items) in
  let overflow = Icon_button.make
      ~icon:Icon.SVG.(make_simple Path.dots_vertical)#root
      () in
  Overflow_menu.make
    ~actions:(List.map Widget.root actions)
    ~overflow:overflow#root
    ~menu
    ()

(** Switches top app bar between contextual action
    mode and normal mode *)
let transform_top_app_bar
    ?(actions = [])
    ?(title : string option)
    ~(class_ : string)
    (scaffold : Scaffold.t) =
  match scaffold#top_app_bar with
  | None -> fun () -> ()
  | Some x ->
    let prev_title = x#title in
    let prev_actions = x#actions in
    Utils.Option.iter x#set_title title;
    x#add_class class_;
    x#set_actions @@ List.map Widget.root actions;
    List.iter Widget.layout actions;
    (fun () ->
       scaffold#set_on_navigation_icon_click_default ();
       List.iter Widget.destroy actions;
       x#set_title prev_title;
       x#set_actions prev_actions;
       x#remove_class class_)
