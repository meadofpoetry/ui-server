open Js_of_ocaml
open Js_of_ocaml_tyxml
include Data_table
module Fmt_js = Markup_js.Fmt

let ( % ) f g x = f (g x)

module Selector = struct
  include Data_table.Selector

  let cell = "." ^ CSS.cell
end

let get_cell_value : type a. a Fmt_js.t -> Dom_html.tableCellElement Js.t -> a =
 fun fmt cell ->
  match fmt with
  | Html _ -> (
    match Element.children cell with
    | [] -> failwith (CSS.root ^ ": failed getting `HTML` cell value - cell is empty")
    | hd :: _ -> Tyxml_js.Html.toelt (Tyxml_js.Of_dom.of_element hd))
  | Custom_elt x -> (
    match Element.children cell with
    | [] ->
        failwith (CSS.root ^ ": failed getting `Custom_elt` cell value - cell is empty")
    | hd :: _ -> x.of_elt @@ Tyxml_js.Html.toelt @@ Tyxml_js.Of_dom.of_element hd)
  | fmt ->
      let s = Js.Opt.case cell##.textContent (fun () -> "") Js.to_string in
      Fmt_js.of_string fmt s

let rec set_cell_value :
    type a. a Fmt_js.t -> a -> Dom_html.tableCellElement Js.t -> unit =
 fun fmt v cell ->
  match fmt with
  | Option (fmt, default) -> (
    match v with
    | None -> set_cell_value String default cell
    | Some x -> set_cell_value fmt x cell)
  | Custom_elt x ->
      Element.remove_children cell;
      Dom.appendChild cell (x.to_elt v)
  | Html _ ->
      Element.remove_children cell;
      Dom.appendChild cell v
  | fmt ->
      let value = Js.string @@ Fmt_js.to_string fmt v in
      cell##.textContent := Js.some value

let compare_cells fmt (a : Dom_html.tableCellElement Js.t as 'a) (b : 'a) =
  Fmt_js.compare fmt (get_cell_value fmt a) (get_cell_value fmt b)

let handle_sort (cell : Dom_html.element Js.t) =
  match Element.get_attribute cell "aria-sort" with
  | None -> Dsc
  | Some order -> (
    match sort_of_string order with
    | Some Dsc -> Asc
    | _ -> Dsc)

class ['a] t ~(fmt : 'a Fmt_js.format) (elt : Dom_html.element Js.t) () =
  object (self)
    inherit Data_table.t elt () as super

    val mutable fmt : 'a Fmt_js.format = fmt

    method insert_row i (data : 'a Fmt_js.data) : Dom_html.tableRowElement Js.t =
      let cells = Markup_js.create_cells_of_fmt fmt data in
      let row = super#tbody##insertRow i in
      Element.add_class row CSS.row;
      List.iter (Dom.appendChild row % Tyxml_js.To_dom.of_td) cells;
      row
    (** Returns an [tableRowElement] representing a new row of the table.
        It inserts it in the rows collection immediately before the [<tr>] element
        at the given index position. If necessary, a [<tbody>] is created.
        If the index is [-1], the new row is appended to the collection.
        If the index is smaller than [-1] or greater than the number of rows
        in the collection, a [DOMException] with the value [IndexSizeError]
        is raised. *)

    method set_row_data_some
        (data : 'a Fmt_js.opt_data)
        (row : Dom_html.tableRowElement Js.t) =
      let cells = row##.cells in
      let rec loop : type a. int -> a Fmt_js.format -> a Fmt_js.opt_data -> unit =
       fun i format data ->
        match format, data with
        | [], [] -> ()
        | fmt :: l1, value :: l2 ->
            let cell = Js.Opt.get (cells##item i) (fun () -> assert false) in
            (match value with
            | None -> ()
            | Some v -> set_cell_value fmt.format v cell);
            loop (succ i) l1 l2
      in
      loop 0 fmt data

    method set_row_data (data : 'a Fmt_js.data) (row : Dom_html.tableRowElement Js.t) =
      let cells = row##.cells in
      let rec loop : type a. int -> a Fmt_js.format -> a Fmt_js.data -> unit =
       fun i format data ->
        match format, data with
        | [], [] -> ()
        | fmt :: l1, value :: l2 ->
            let cell = Js.Opt.get (cells##item i) (fun () -> assert false) in
            set_cell_value fmt.format value cell;
            loop (succ i) l1 l2
      in
      loop 0 fmt data

    method get_row_data (row : Dom_html.tableRowElement Js.t) =
      let cells = row##.cells in
      let rec loop : type a. int -> a Fmt_js.format -> a Fmt_js.data =
       fun i format ->
        match format with
        | [] -> []
        | column :: tl ->
            let cell = Js.Opt.get (cells##item i) (fun () -> assert false) in
            let value = get_cell_value column.format cell in
            value :: loop (succ i) tl
      in
      loop 0 fmt

    method get_row_data_lazy (row : Dom_html.tableRowElement Js.t) =
      let cells = row##.cells in
      let rec loop : type a. int -> a Fmt_js.format -> a Fmt_js.data_lazy =
       fun i format ->
        match format with
        | [] -> []
        | column :: tl ->
            let cell = Js.Opt.get (cells##item i) (fun () -> assert false) in
            (fun () -> get_cell_value column.format cell) :: loop (succ i) tl
      in
      loop 0 fmt

    method dump : 'a Fmt_js.data list = List.map self#get_row_data super#rows
    (** Return table formatted data. *)
  end

let attach ~fmt (elt : #Dom_html.element Js.t) : 'a t =
  new t ~fmt (elt :> Dom_html.element Js.t) ()

let make ?classes ?attrs ?dense ?(data = []) ~format () =
  Markup_js.create_of_fmt ?classes ?attrs ?dense ~format ~data ()
  |> Tyxml_js.To_dom.of_div
  |> attach ~fmt:format

(** Example using GADT format:

   {[ let table =
        let (fmt : _ Fmt_js.format) =
          Fmt_js.
            [ make_column ~title:"Title 1" Int
            ; make_column ~title:"Title 2" Int
            ; make_column ~title:"Title 3" Int ]
        in
        let (data : _ Fmt_js.data list) =
          [[3; 3; 3]; [4; 5; 4]; [1; 2; 3]; [1; 1; 1]; [4; 3; 1]; [1; 6; 4]]
        in
        make ~format:fmt ~data ()
   ]}
*)
