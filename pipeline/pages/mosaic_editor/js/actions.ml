open Js_of_ocaml
open Js_of_ocaml_lwt
open Components

let ( >>= ) = Lwt.bind

type 'a t =
  { menu : Item_list.Item.t
  ; icon : Icon_button.t
  ; active : ('a -> bool) option
  ; callback : ('a -> Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  }

let make_icon_button ~icon name =
  let icon =
    Icon_button.make
      ~classes:[Top_app_bar.CSS.action_item]
      ~icon:(Icon.SVG.make_simple icon)#root
      () in
  icon#set_attribute "title" name;
  icon

let make_menu_item ~icon name =
  Item_list.Item.make
    ~role:"menuitem"
    ~graphic:Icon.SVG.(make_simple icon) (* FIXME *)
    name

let make ?active ~name ~icon ~callback () =
  { menu = make_menu_item ~icon name
  ; icon = make_icon_button ~icon name
  ; active
  ; callback
  }

let make_overflow_menu
    ?body
    ?viewport
    (state : 'a React.signal)
    (actions : 'a t list) =
  let menu =
    Menu.make_of_item_list ?body ?viewport
    @@ Item_list.make
    @@ List.map (fun x -> x.menu) actions in
  let overflow = Icon_button.make
      ~icon:Icon.SVG.(make_simple Path.dots_vertical)#root
      () in
  let elt =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Components_lab.Overflow_menu.Markup.create
      ~menu:menu#markup
      ~actions:(List.map (fun x -> x.icon#markup) actions)
      ~overflow:overflow#markup
      () in
  object
    inherit Components_lab.Overflow_menu.t ~resize_handler:false elt () as super

    val _s = React.S.map (fun state ->
        List.iter (fun { menu; icon; active; _ } ->
            let display = match active with
              | None -> ""
              | Some f -> if f state then "" else "none" in
            menu#root##.style##.display := Js.string display;
            icon#root##.style##.display := Js.string display)
          actions) state

    method! initial_sync_with_dom () : unit =
      let action_listeners =
        List.map (fun { callback; icon; _ } ->
            Lwt_js_events.clicks icon#root (fun e ->
                callback (React.S.value state) (e :> Dom_html.event Js.t)))
          actions in
      listeners <- Lwt_js_events.(
          [ seq_loop (make_event Menu.Event.selected) menu#root (fun e _ ->
                let detail = Widget.event_detail e in
                let button = (List.nth actions detail##.index).icon in
                Js.Opt.iter (Dom_html.CoerceTo.button button#root)
                  (fun (button : Dom_html.buttonElement Js.t) -> button##click);
                Lwt.return_unit) ]
          @ action_listeners
          @ listeners);
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      menu#destroy ();
      overflow#destroy ();
      List.iter (fun { menu; icon; _ } ->
          menu#destroy ();
          icon#destroy ())
        actions;
      React.S.stop ~strong:true _s;
      super#destroy ()
  end

let get_cell' f = function
  | [] -> invalid_arg "list is empty"
  | x :: tl ->
    snd @@ List.fold_left (fun ((pos', _) as acc) x ->
        let pos = Grid.Util.get_cell_position x in
        if f pos' pos then (pos, x) else acc)
      (Grid.Util.get_cell_position x, x) tl

let get_topmost_cell cells =
  get_cell' (fun acc pos -> pos.row < acc.row) cells

let get_bottommost_cell cells =
  get_cell' (fun acc pos -> pos.row > acc.row) cells

let get_leftmost_cell cells =
  get_cell' (fun acc pos -> pos.col < acc.col) cells

let get_rightmost_cell cells =
  get_cell' (fun acc pos -> pos.col > acc.col) cells

(** Switches top app bar between contextual action
    mode and normal mode *)
let transform_top_app_bar
    ?(title : string option)
    ?(class_ : string option)
    ?on_navigation_icon_click
    (scaffold : Scaffold.t) =
  match scaffold#top_app_bar with
  | None -> fun () -> ()
  | Some x ->
    let prev_nav_icon_click = scaffold#on_navigation_icon_click in
    let prev_title = x#title in
    Option.iter scaffold#set_on_navigation_icon_click on_navigation_icon_click;
    Option.iter x#set_title title;
    Option.iter x#add_class class_;
    (fun () ->
       (match prev_nav_icon_click with
        | None -> scaffold#set_on_navigation_icon_click_default ()
        | Some f -> scaffold#set_on_navigation_icon_click f);
       x#set_title prev_title;
       Option.iter x#remove_class class_)

module Undo = struct
  let undo (undo_manager : Undo_manager.t) =
    make ~callback:(fun _ _ _ -> Undo_manager.undo undo_manager; Lwt.return_unit)
      ~active:(fun _ -> Undo_manager.has_undo undo_manager)
      ~name:"Отменить"
      ~icon:Icon.SVG.Path.undo
      ()

  let redo (undo_manager : Undo_manager.t) =
    make ~callback:(fun _ _ _ -> Undo_manager.redo undo_manager; Lwt.return_unit)
      ~active:(fun _ -> Undo_manager.has_redo undo_manager)
      ~name:"Повторить"
      ~icon:Icon.SVG.Path.redo
      ()
end

module Container_actions = struct
  type state = Dom_html.element Js.t list

  let wizard (wizard : Pipeline_widgets.Wizard.t) (grid : Grid.t) =
    make ~callback:(fun _ _ _ ->
        wizard#open_await ()
        >>= function
        | Close | Destroy | Custom _ -> Lwt.return_unit
        | Accept ->
          let open Pipeline_types in
          let wm = wizard#value in
          let wm = Wm.Annotated.annotate ~active:wm ~stored:wm in
          let grid_props = Container_utils.grid_properties_of_layout wm in
          let cells = List.map (fun (id, (container, pos)) ->
              Js_of_ocaml_tyxml.Tyxml_js.(
                To_dom.of_element
                @@ Grid.Markup.create_cell
                  ~attrs:Html.([a_user_data "title" id])
                  ~content:(Container_utils.content_of_container container)
                  pos))
              grid_props.cells in
          grid#reset ~cells
            ~rows:(`Value grid_props.rows)
            ~cols:(`Value grid_props.cols)
            ();
          Lwt.return_unit)
      ~active:(function [] -> true | _ -> false)
      ~name:"Мастер"
      ~icon:Icon.SVG.Path.auto_fix
      ()

  let description (input : string Textfield.t) (dialog : Dialog.t) =
    make ~callback:(fun selected _ _ ->
        match selected with
        | [cell] ->
          let title = Container_utils.get_cell_title cell in
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
             then Container_utils.set_cell_title cell value';
             Lwt.return_unit)
        | _ -> Lwt.return_unit)
      ~active:(function [_] -> true | _ -> false)
      ~name:"Описание"
      ~icon:Icon.SVG.Path.information
      ()

  let edit ~edit_container =
    make ~callback:(fun selected _ _ ->
        match selected with
        | [cell] -> edit_container cell
        | _ -> Lwt.return_unit)
      ~active:(function [_] -> true | _ -> false)
      ~name:"Редактировать"
      ~icon:Icon.SVG.Path.pencil
      ()

  let merge ~on_remove undo_manager grid =
    make ~callback:(fun selected _ _ ->
        match grid#merge selected with
        | None -> Lwt.return_unit
        | Some merged ->
          on_remove ();
          let v =
            { Undo_manager.
              undo = (fun () ->
                  Dom.removeChild grid#root merged;
                  List.iter (Dom.appendChild grid#root) selected)
            ; redo = (fun () ->
                  List.iter (Dom.removeChild grid#root) selected;
                  Dom.appendChild grid#root merged)
            } in
          Undo_manager.add undo_manager v;
          Lwt.return_unit)
      ~active:(function [] | [_] -> false | _ -> true)
      ~name:"Объединить ячейки"
      ~icon:Icon.SVG.Path.table_merge_cells
      ()

  let add_row_above grid =
    make ~callback:(fun selected _ _ ->
        match selected with
        | [] -> Lwt.return_unit
        | cells ->
          let cell = get_topmost_cell cells in
          grid#add_row_before cell;
          Lwt.return_unit)
      ~active:(function [] -> false | _ -> true)
      ~name:"Добавить ряд сверху"
      ~icon:Icon.SVG.Path.table_row_plus_before
      ()

  let add_row_below grid =
    make ~callback:(fun selected _ _ ->
        match selected with
        | [] -> Lwt.return_unit
        | cells ->
          let cell = get_bottommost_cell cells in
          grid#add_row_after cell;
          Lwt.return_unit)
      ~active:(function [] -> false | _ -> true)
      ~name:"Добавить ряд снизу"
      ~icon:Icon.SVG.Path.table_row_plus_after
      ()

  let remove_row ~on_remove grid =
    make ~callback:(fun selected _ _ ->
        let _, cells =
          List.split
          @@ List.fold_left (fun acc x ->
              let pos = Grid.Util.get_cell_position x in
              if List.mem_assoc pos.row acc
              then acc else (pos.row, x) :: acc)
            [] selected in
        List.iter grid#remove_row cells;
        on_remove ();
        Lwt.return_unit)
      ~active:(function [] -> false | _ -> true)
      ~name:"Удалить ряд"
      ~icon:Icon.SVG.Path.table_row_remove
      ()

  let add_col_left (grid : Grid.t) =
    make ~callback:(fun selected _ _ ->
        match selected with
        | [] -> Lwt.return_unit
        | cells ->
          grid#add_column_before @@ get_leftmost_cell cells;
          Lwt.return_unit)
      ~active:(function [] -> false | _ -> true)
      ~name:"Добавить столбец слева"
      ~icon:Icon.SVG.Path.table_column_plus_before
      ()

  let add_col_right grid =
    make ~callback:(fun selected _ _ ->
        match selected with
        | [] -> Lwt.return_unit
        | cells ->
          grid#add_column_after @@ get_rightmost_cell cells;
          Lwt.return_unit)
      ~active:(function [] -> false | _ -> true)
      ~name:"Добавить столбец справа"
      ~icon:Icon.SVG.Path.table_column_plus_after
      ()

  let remove_col ~on_remove grid =
    make ~callback:(fun selected _ _ ->
        let _, cells =
          List.split
          @@ List.fold_left (fun acc x ->
              let pos = Grid.Util.get_cell_position x in
              if List.mem_assoc pos.col acc
              then acc else (pos.col, x) :: acc)
            [] selected in
        List.iter grid#remove_column cells;
        on_remove ();
        Lwt.return_unit)
      ~active:(function [] -> false | _ -> true)
      ~name:"Удалить столбец"
      ~icon:Icon.SVG.Path.table_column_remove
      ()

  let make_menu ~on_remove ~edit_container
      s_state undo_manager wizard_widget grid =
    let body = Dom_html.document##.body in
    let textfield, dialog = Container_utils.UI.make_description_dialog () in
    Dom.appendChild body dialog#root;
    let menu = make_overflow_menu s_state
        [ Undo.undo undo_manager
        ; Undo.redo undo_manager
        ; wizard wizard_widget grid
        ; edit ~edit_container
        ; description textfield dialog
        ; merge ~on_remove undo_manager grid
        ; add_row_above grid
        ; add_row_below grid
        ; remove_row ~on_remove grid
        ; add_col_left grid
        ; add_col_right grid
        ; remove_col ~on_remove grid
        ] in
    menu#set_on_destroy (fun () ->
        Element.remove_child_safe body dialog#root;
        textfield#destroy ();
        dialog#destroy ());
    menu
end
