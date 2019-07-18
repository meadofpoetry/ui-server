open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Pipeline_types

module Attr = struct
  let title = "data-title"
  let aspect = "data-aspect"
end

let get_cell_title (cell : Dom_html.element Js.t) : string =
  match Element.get_attribute cell Attr.title with
  | None -> ""
  | Some s -> s

let set_cell_title (cell : Dom_html.element Js.t) (title : string) : unit =
  Element.set_attribute cell Attr.title title

let cell_title_prefix = "Контейнер #"

let cell_title (i : int) =
  Printf.sprintf "Контейнер #%d" i

let find_min_spare ?(min = 0) l =
  let rec aux acc = function
    | [] -> acc
    | x :: tl -> if acc = x then aux (succ x) tl else acc in
  aux min l

let gen_cell_title (cells : Dom_html.element Js.t list) =
  let titles = List.map get_cell_title cells in
  let indexes =
    List.sort_uniq compare
    @@ List.fold_left (fun acc s ->
        try
          let start = String.length cell_title_prefix in
          let len = String.length s - start in
          let s' = String.sub s start len in
          (int_of_string s') :: acc
        with exn -> acc)
      [] titles in
  cell_title (find_min_spare ~min:1 indexes)

let sum x = Array.fold_left (fun acc -> function
    | Grid.Fr x -> acc +. x
    | _ -> acc) 0. x

let fr_to_px fr px =
  (float_of_int px) /. (sum fr)

let cell_position_to_wm_position
    ~px_in_fr_w
    ~px_in_fr_h
    ~cols
    ~rows
    { Grid. row; col; row_span; col_span } : Wm.position =
  let get_cell_size ?(start = 0) ~len side =
    sum @@ Array.sub side start len in
  { x = (get_cell_size ~len:(pred col) cols) *. px_in_fr_w
  ; y = (get_cell_size ~len:(pred row) rows) *. px_in_fr_h
  ; w = (get_cell_size ~start:(pred col) ~len:col_span cols) *. px_in_fr_w
  ; h = (get_cell_size ~start:(pred row) ~len:row_span rows) *. px_in_fr_h
  }

type grid_properties =
  { rows : Grid.value list
  ; cols : Grid.value list
  ; cells : (string * (Wm.Annotated.container * Grid.cell_position)) list
  }

let swap (a : Dom_html.element Js.t as 'a) (b : 'a) : unit =
  let get_attr attr e =
    Js.Opt.get
      (e##getAttribute (Js.string attr))
      (fun () -> Js.string "") in
  let swap_class e =
    if Element.has_class a Grid.CSS.cell_selected
    then (Element.add_class b Grid.CSS.cell_selected;
          Element.remove_class a Grid.CSS.cell_selected) in
  let id, title, aspect, html =
    a##.id
  , get_attr Attr.title a
  , get_attr Attr.aspect a
  , a##.innerHTML in
  a##.id := b##.id;
  a##setAttribute
    (Js.string Attr.title)
    (get_attr Attr.title b);
  a##.innerHTML := b##.innerHTML;
  b##.id := id;
  b##setAttribute (Js.string Attr.title) title;
  b##.innerHTML := html;
  swap_class a;
  swap_class b

let first_grid_index = 1

let rec get_deltas points =
  List.rev @@ snd @@ List.fold_left (fun (prev, deltas) x ->
      let delta = x -. prev in
      (prev +. delta, delta :: deltas)) (0., []) points

let points_to_frs resolution (deltas : float list) : (Grid.value list) =
  let len = float_of_int @@ List.length deltas in
  try List.map (fun x -> Grid.Fr (x /. resolution *. len)) deltas
  with Division_by_zero -> []

let get_cell_pos_part (edge : float) points =
  let rec aux cnt = function
    | [] -> failwith "Corresponding cell edge not found" (* FIXME *)
    | x :: _ when x = edge -> cnt
    | _ :: tl -> aux (cnt + 1) tl in
  if edge = 0. then first_grid_index
  else aux (first_grid_index + 1) points

let get_cell_positions ~lefts ~tops =
  List.map (fun (id, _, ({ Wm.Annotated. position = x; _ } as c)) ->
      let col = get_cell_pos_part x.x lefts in
      let row = get_cell_pos_part x.y tops in
      let col_end = get_cell_pos_part (x.x +. x.w) lefts in
      let row_end = get_cell_pos_part (x.y +. x.h) tops in
      id, (c, { Grid.
                col
              ; row
              ; col_span = col_end - col
              ; row_span = row_end - row
              }))

let grid_properties_of_layout ({ resolution = w, h; layout; _ } : Wm.Annotated.t) =
  let sort = List.sort_uniq compare in
  let lefts, tops =
    List.split @@ List.map (fun (_, _, (c : Wm.Annotated.container)) ->
        (c.position.w +. c.position.x),
        (c.position.h +. c.position.y)) layout
    |> fun (x, y) -> sort x, sort y in
  let cols = points_to_frs (float_of_int w) (get_deltas lefts) in
  let rows = points_to_frs (float_of_int h) (get_deltas tops) in
  let cells = get_cell_positions ~lefts ~tops layout in
  { rows; cols; cells }

module UI = struct

  open Js_of_ocaml_lwt

  let ( >>= ) = Lwt.bind

  let make_input ~label () : int Textfield.t =
    Textfield.make_textfield
      ~label
      (Integer (Some 1, None))

  let make_empty_placeholder
      (wizard_dialog : Wizard.t)
      (table_dialog, value : Dialog.t * (unit -> int option * int option))
      (grid : Grid.t) =
    let table =
      Icon_button.make
        ~on_click:(fun _ _ ->
            table_dialog#open_await ()
            >>= function
            | Close | Destroy | Custom _ -> Lwt.return_unit
            | Accept ->
              match value () with
              | None, _ | _, None -> Lwt.return_unit
              | Some cols, Some rows ->
                grid#reset ~cols ~rows ();
                Lwt.return_unit)
        ~icon:Icon.SVG.(make_simple Path.table_plus)#root
        () in
    let wizard =
      Icon_button.make
        ~on_click:(fun _ _ ->
            wizard_dialog#open_await ()
            >>= function
            | Close | Destroy | Custom _ -> Lwt.return_unit
            | Accept -> Lwt.return_unit)
        ~icon:Icon.SVG.(make_simple Path.auto_fix)#root
        () in
    let content = Box.make ~dir:`Row [wizard; table] in
    Ui_templates.Placeholder.With_icon.make
      ~font:Body_1
      ~icon:content#root
      ~text:"Мозаика пуста. \n\
             Воспользуйтесь мастером настройки \n\
             или начните с создания таблицы!"
      ()

  let add_table_dialog () =
    let cols = make_input ~label:"Число столбцов" () in
    let rows = make_input ~label:"Число строк" () in
    let title =
      Tyxml_js.To_dom.of_element
      @@ Dialog.Markup.create_title_simple
        ~title:"Добавление таблицы"
        () in
    let content =
      Tyxml_js.To_dom.of_element
      @@ Dialog.Markup.create_content
        ~classes:[Box.CSS.root; Box.CSS.vertical]
        ~content:[cols#markup; rows#markup]
        () in
    let submit =
      Button.attach
      @@ Dialog.make_action ~action:Accept ~label:"Применить" () in
    let actions = Dialog.(
        [ make_action ~action:Close ~label:"Отмена" ()
        ; Js.Unsafe.coerce submit#root
        ]) in
    let check_input () = match cols#value, rows#value with
      | None, _ | _, None -> submit#set_disabled true
      | Some _, Some _ -> submit#set_disabled false in
    let listeners =
      Lwt_js_events.(
        [ inputs cols#input_element (fun _ _ -> check_input (); Lwt.return_unit)
        ; inputs rows#input_element (fun _ _ -> check_input (); Lwt.return_unit)
        ]) in
    let dialog = Dialog.make
        ~classes:[Page_mosaic_editor_tyxml.Container_editor.CSS.dialog_add_table]
        ~title ~content ~actions () in
    (* Set initial submit button state *)
    check_input ();
    (* Clear event listeners on destroy *)
    dialog#set_on_destroy (fun () ->
        List.iter Lwt.cancel listeners);
    dialog, (fun () -> cols#value, rows#value)

end
