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
    let scale_factor = self#scale_factor in
    let width' = int_of_float @@ float_of_int (fst resolution) *. scale_factor in
    let height' = int_of_float @@ float_of_int (snd resolution) *. scale_factor in
    grid#root##.style##.width := Utils.px_js width';
    grid#root##.style##.height := Utils.px_js height';
    super#layout ()

  method! destroy () : unit =
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    super#destroy ()

  method toolbar = _toolbar

  (* Private methods *)

  method private create_grid_actions () =
    let make_action path =
      Icon_button.make
        ~icon:Icon.SVG.(make_simple path)
        ~on_click:(fun _ _ -> Lwt.return_unit)
        () in
    let add_row_above =
      make_action Icon.SVG.Path.table_row_plus_before in
    let add_row_below =
      make_action Icon.SVG.Path.table_row_plus_after in
    let add_col_left =
      make_action Icon.SVG.Path.table_column_plus_before in
    let add_col_right =
      make_action Icon.SVG.Path.table_column_plus_after in
    [ add_row_above
    ; add_row_below
    ; add_col_left
    ; add_col_right
    ]

  method private handle_selected e _ : unit Lwt.t =
    Js.Unsafe.global##.console##log e##.detail |> ignore;
    Lwt.return_unit

  method private handle_keydown _ _ : unit Lwt.t =
    (* TODO implement *)
    Lwt.return_unit

  method private scale_factor : float =
    Js.Opt.case (Element.get_parent grid#root)
      (fun () -> 0.)
      (fun parent ->
         let cur_width = float_of_int parent##.offsetWidth in
         let cur_height = float_of_int parent##.offsetHeight in
         let cur_aspect = cur_width /. cur_height in
         if cur_aspect > (float_of_int (fst resolution) /. float_of_int (snd resolution))
         then cur_height /. float_of_int (snd resolution)
         else cur_width /. float_of_int (fst resolution))

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
      ~grid () in
  new t
    ~resolution:wm.resolution
    ~containers:wm.layout
    elt ()
