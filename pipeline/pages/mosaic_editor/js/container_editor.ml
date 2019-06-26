open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Pipeline_types

(* TODO
   [ ] undo/redo
   [ ] selection
   [ ] editing mode: table, containers
*)

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.bind

module Selector = struct
  let grid = Printf.sprintf ".%s" Resizable_grid.CSS.grid
  let actions = Printf.sprintf ".%s" Card.CSS.action_icons
end

class t ?(containers = []) ~resolution elt () = object(self)
  val ghost = Dom_html.(createDiv document)
  val actions = match Element.query_selector elt Selector.actions with
    | None -> failwith "container-editor: actions element not found"
    | Some x -> x
  val grid = match Element.query_selector elt Selector.grid with
    | None -> failwith "container-editor: grid element not found"
    | Some x -> Resizable_grid.attach ~drag_interval:(Fr 0.1) x

  val mutable _containers : (string * Wm.container) list = containers
  val mutable _listeners = []
  val mutable _focused_item = None
  val mutable _toolbar = Widget.create_div ()

  inherit Drop_target.t elt () as super

  method! init () : unit =
    grid#reset ~rows:5 ~cols:5 ();
    List.iter (Element.append_child actions % Widget.root) @@ self#create_grid_actions ();
    super#init ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Events.(
        [ keydowns super#root self#handle_keydown
        ; listen_lwt grid#root Resizable_grid.Event.selected self#handle_selected
        ]);
    super#initial_sync_with_dom ()

  method! layout () : unit =
    let parent_width, parent_height =
      Js.Opt.case (Element.get_parent grid#root)
        (fun () -> 0, 0)
        (fun parent -> parent##.offsetWidth, parent##.offsetHeight) in
    let width = parent_height * (fst resolution) / (snd resolution) in
    grid#root##.style##.width := Utils.px_js width;
    grid#root##.style##.height := Utils.px_js parent_height;
    let force = parent_width - 2 > width in
    grid#toggle_class ~force Resizable_grid.CSS.grid_bordered;
    super#layout ()

  method! destroy () : unit =
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    super#destroy ()

  method toolbar = _toolbar

  (* Private methods *)

  method private create_grid_actions () =
    let make_action ~on_click path =
      Icon_button.make
        ~on_click
        ~icon:Icon.SVG.(make_simple path)
        () in
    let add_row_above =
      make_action
        ~on_click:(fun _ _ ->
            grid#add_row_before (List.hd grid#selected_cells);
            Lwt.return_unit)
        Icon.SVG.Path.table_row_plus_before in
    let add_row_below =
      make_action
        ~on_click:(fun _ _ ->
            grid#add_row_after (List.hd grid#selected_cells);
            Lwt.return_unit)
        Icon.SVG.Path.table_row_plus_after in
    let remove_row =
      make_action
        ~on_click:(fun _ _ ->
            grid#remove_row (List.hd grid#selected_cells);
            Lwt.return_unit)
        Icon.SVG.Path.table_row_remove in
    let add_col_left =
      make_action
        ~on_click:(fun _ _ ->
            grid#add_column_before (List.hd grid#selected_cells);
            Lwt.return_unit)
        Icon.SVG.Path.table_column_plus_before in
    let add_col_right =
      make_action
        ~on_click:(fun _ _ ->
            grid#add_column_after (List.hd grid#selected_cells);
            Lwt.return_unit)
        Icon.SVG.Path.table_column_plus_after in
    let remove_col =
      make_action
        ~on_click:(fun _ _ ->
            grid#remove_column (List.hd grid#selected_cells);
            Lwt.return_unit)
        Icon.SVG.Path.table_column_remove in
    [ add_row_above
    ; add_row_below
    ; remove_row
    ; add_col_left
    ; add_col_right
    ; remove_col
    ]

  method private handle_selected e _ : unit Lwt.t =
    Js.Unsafe.global##.console##log e##.detail |> ignore;
    Lwt.return_unit

  method private handle_keydown _ _ : unit Lwt.t =
    (* TODO implement *)
    Lwt.return_unit

  method private handle_dropped_json (json : Yojson.Safe.json) : unit Lwt.t =
    Lwt.return_unit

  method private move_ghost ?aspect event : unit =
    ()
end

let make (wm : Wm.t) =
  let cells = Resizable_grid_utils.gen_cells
      ~f:(fun ~col ~row () -> Resizable_grid.Markup.create_grid_cell ~col ~row ())
      ~cols:5
      ~rows:5 in
  let grid = Resizable_grid.Markup.create_grid
      ~rows:5
      ~cols:5
      ~content:cells
      () in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Resizable_grid.Markup.create
      ~classes:[Card.CSS.root]
      ~width:(fst wm.resolution)
      ~height:(snd wm.resolution)
      ~grid () in
  new t
    ~resolution:wm.resolution
    ~containers:wm.layout
    elt ()
