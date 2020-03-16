open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Pipeline_types
include Page_mosaic_editor_tyxml.Container_editor
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

module Attr = struct
  let title = "data-title"

  let aspect = "data-aspect"
end

let ( = ) (x : int) y = x = y

let equal_float ?(epsilon = epsilon_float) a b = abs_float (a -. b) < epsilon

let set_cell_title (cell : Dom_html.element Js.t) (title : string) : unit =
  Element.set_attribute cell Attr.title title

let cell_title_prefix = "Контейнер #"

let cell_title (i : int) = Printf.sprintf "%s%d" cell_title_prefix i

let find_min_spare ?(min = 0) l =
  let rec aux acc = function
    | [] -> acc
    | x :: tl -> if acc = x then aux (succ x) tl else acc
  in
  aux min l

let get_cell_title (cell : Dom_html.element Js.t) : string =
  match Element.get_attribute cell Attr.title with None -> "" | Some x -> x

let gen_cell_title (cells : Dom_html.element Js.t list) =
  let titles = List.map get_cell_title cells in
  let indexes =
    List.sort_uniq compare
    @@ List.fold_left
         (fun acc s ->
           try
             let start = String.length cell_title_prefix in
             let len = String.length s - start in
             let s' = String.sub s start len in
             int_of_string s' :: acc
           with _ -> acc)
         [] titles
  in
  cell_title (find_min_spare ~min:1 indexes)

let sum x =
  Array.fold_left (fun acc -> function Grid.Fr x -> acc +. x | _ -> acc) 0. x

let cell_position_to_wm_position ~cols ~rows
    { Grid.row; col; row_span; col_span } : Wm.position =
  let total_w = sum cols in
  let total_h = sum rows in
  let get_cell_size ?(start = 0) ~len side = sum @@ Array.sub side start len in
  Position.Normalized.validate
  @@ {
       x = get_cell_size ~len:(pred col) cols /. total_w;
       y = get_cell_size ~len:(pred row) rows /. total_h;
       w = get_cell_size ~start:(pred col) ~len:col_span cols /. total_w;
       h = get_cell_size ~start:(pred row) ~len:row_span rows /. total_h;
     }

let content_of_container (container : Wm.Annotated.container) =
  let widgets = List.map D.create_widget container.widgets in
  [ D.create_widget_wrapper widgets ]

type grid_properties = {
  rows : Grid.value list;
  cols : Grid.value list;
  cells : (string * (Wm.Annotated.container * Grid.cell_position)) list;
}

let swap (a : Dom_html.element Js.t as 'a) (b : 'a) : unit =
  let apos = Grid.Util.get_cell_position a in
  let bpos = Grid.Util.get_cell_position b in
  Grid.Util.set_cell_position apos b;
  Grid.Util.set_cell_position bpos a

let first_grid_index = 1

let get_deltas points =
  List.rev
  @@ snd
  @@ List.fold_left
       (fun (prev, deltas) x ->
         let delta = x -. prev in
         (prev +. delta, delta :: deltas))
       (0., []) points

let points_to_frs (deltas : float list) : Grid.value list =
  let len = float_of_int @@ List.length deltas in
  try List.map (fun x -> Grid.Fr (x *. len)) deltas
  with Division_by_zero -> []

let get_cell_pos_part (edge : float) points =
  let rec aux cnt = function
    | [] -> failwith "Corresponding cell edge not found" (* FIXME *)
    | x :: _ when equal_float x edge -> cnt
    | _ :: tl -> aux (cnt + 1) tl
  in
  if equal_float edge 0. then first_grid_index
  else aux (first_grid_index + 1) points

let get_cell_positions ~lefts ~tops =
  List.map (fun (id, _, ({ Wm.Annotated.position = x; _ } as c)) ->
      let col = get_cell_pos_part x.x lefts in
      let row = get_cell_pos_part x.y tops in
      let col_end = get_cell_pos_part (x.x +. x.w) lefts in
      let row_end = get_cell_pos_part (x.y +. x.h) tops in
      ( id,
        ( c,
          { Grid.col; row; col_span = col_end - col; row_span = row_end - row }
        ) ))

let grid_properties_of_layout ({ layout; _ } : Wm.Annotated.t) =
  let sort = List.sort_uniq compare in
  let lefts, tops =
    List.split
    @@ List.map
         (fun (_, _, (c : Wm.Annotated.container)) ->
           (c.position.w +. c.position.x, c.position.h +. c.position.y))
         layout
    |> fun (x, y) -> (sort x, sort y)
  in
  let cols = points_to_frs (get_deltas lefts) in
  let rows = points_to_frs (get_deltas tops) in
  let cells = get_cell_positions ~lefts ~tops layout in
  { rows; cols; cells }

module UI = struct
  open Js_of_ocaml_lwt

  let ( >>= ) = Lwt.bind

  let make_input ~label () : int Textfield.t =
    Textfield.make ~label ~validation:(Integer (Some 1, None)) ()

  let make_empty_placeholder wizard_dialog
      ((table_dialog, value) : Dialog.t * (unit -> int option * int option))
      (grid : Grid.t) =
    let table =
      Icon_button.make
        ~on_click:(fun _ _ ->
          table_dialog#open_await () >>= function
          | Close | Destroy | Custom _ -> Lwt.return_unit
          | Accept -> (
              match value () with
              | None, _ | _, None -> Lwt.return_unit
              | Some cols, Some rows ->
                  grid#reset
                    ~cols:(`Repeat (cols, Fr 1.))
                    ~rows:(`Repeat (rows, Fr 1.))
                    ();
                  Lwt.return_unit ))
        ~icon:Icon.SVG.(D.icon ~d:Path.table_plus ())
        ()
    in
    let wizard =
      Icon_button.make
        ~on_click:(fun _ _ ->
          wizard_dialog#open_await () >>= function
          | Dialog.Close | Destroy | Custom _ -> Lwt.return_unit
          | Accept -> Lwt.return_unit)
        ~icon:Icon.SVG.(D.icon ~d:Path.table_plus ())
        ()
    in
    let content = Box.D.box ~children:[ wizard#markup; table#markup ] () in
    let placeholder =
      Components_lab.Placeholder.make ~icon:content
        ~text:
          (`Text
            "Мозаика пуста. \n\
             Воспользуйтесь мастером настройки \n\
             или начните с создания таблицы!") ()
    in
    placeholder#set_on_destroy (fun () ->
        wizard#destroy ();
        table#destroy ());
    placeholder

  let add_table_dialog () =
    let open Dialog.D in
    let cols = make_input ~label:"Число столбцов" () in
    let rows = make_input ~label:"Число строк" () in
    let title = dialog_title ~title:"Добавление таблицы" () in
    let content =
      dialog_content
        ~classes:[ Box.CSS.root; Box.CSS.vertical ]
        ~children:[ cols#markup; rows#markup ]
        ()
    in
    let submit =
      Button.attach
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ dialog_action ~action:Accept ~label:"Применить" ()
    in
    let actions =
      [ dialog_action ~action:Close ~label:"Отмена" (); submit#markup ]
    in
    let check_input () =
      match (cols#value, rows#value) with
      | None, _ | _, None -> submit#set_disabled true
      | Some _, Some _ -> submit#set_disabled false
    in
    let listeners =
      Lwt_js_events.
        [
          inputs cols#input_element (fun _ _ ->
              check_input ();
              Lwt.return_unit);
          inputs rows#input_element (fun _ _ ->
              check_input ();
              Lwt.return_unit);
        ]
    in
    let dialog =
      Dialog.make
        ~classes:
          [ Page_mosaic_editor_tyxml.Container_editor.CSS.dialog_add_table ]
        ~title ~content ~actions ()
    in
    (* Set initial submit button state *)
    check_input ();
    (* Clear event listeners on destroy *)
    dialog#set_on_destroy (fun () -> List.iter Lwt.cancel listeners);
    (dialog, fun () -> (cols#value, rows#value))

  let make_description_dialog () =
    let open Dialog.D in
    let title = "Описание" in
    let helper_text = Textfield.Helper_text.make ~validation:true ~text:"" () in
    let textfield =
      Textfield.make ~label:"Наименование" ~helper_text
        ~validation:Text ()
    in
    let title = dialog_title ~title () in
    let content =
      dialog_content
        ~children:
          [
            textfield#markup;
            Textfield.D.textfield_helper_line ~children:[ helper_text#markup ]
              ();
          ]
        ()
    in
    let accept =
      Button.attach
      @@ Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ dialog_action ~label:"OK" ~action:Accept ()
    in
    let actions =
      [ dialog_action ~label:"Отмена" ~action:Close (); accept#markup ]
    in
    let dialog = Dialog.make ~title ~content ~actions () in
    (textfield, accept, dialog)

  let make_wizard_dialog streams structure wm =
    let open Dialog.D in
    let wizard = Pipeline_widgets.Wizard.make streams structure wm in
    let title = dialog_title ~title:Pipeline_widgets.Wizard.title () in
    let content = dialog_content ~children:[ wizard#markup ] () in
    let actions =
      [
        dialog_action ~label:"Отмена" ~action:Close ();
        dialog_action ~label:"Применить" ~action:Accept ();
      ]
    in
    let dialog =
      Tyxml_js.To_dom.of_element @@ dialog ~title ~content ~actions ()
    in
    object
      inherit Dialog.t dialog () as super

      method! initial_sync_with_dom () : unit =
        listeners <-
          Js_of_ocaml_lwt.Lwt_js_events.(
            [
              seq_loop (make_event Treeview.Event.action) wizard#root
                (fun _ _ ->
                  super#layout ();
                  Lwt.return_unit);
            ]
            @ listeners);
        super#initial_sync_with_dom ()

      method! destroy () : unit =
        wizard#destroy ();
        super#destroy ()

      method wizard = wizard
    end
end
