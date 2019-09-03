open Js_of_ocaml
open Js_of_ocaml_tyxml

include Components_tyxml.Data_table

module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Fmt = Markup.Fmt

module Selector = struct
  let row = Printf.sprintf ".%s" CSS.row

  let row_checkbox = Printf.sprintf ".%s" CSS.row_checkbox

  let row_selected = Printf.sprintf ".%s" CSS.row_selected

  let header_row_checkbox = Printf.sprintf ".%s" CSS.header_row_checkbox
end

let ( >>= ) = Lwt.bind

type column =
  { sortable : bool
  ; title : string
  }

let make_column ?(sortable = false) ~title () =
  { sortable; title }

let get_cell_value : type a. a Fmt.t -> Dom_html.element Js.t -> a =
  fun fmt cell ->
  match fmt with
  | Html _ ->
    Tyxml_js.Html.toelt
    @@ Tyxml_js.Of_dom.of_element cell
  | Custom_elt x ->
    x.of_elt
    @@ Tyxml_js.Html.toelt
    @@ Tyxml_js.Of_dom.of_element cell
  | fmt ->
    let s = Js.Opt.case cell##.textContent (fun () -> "") Js.to_string in
    Fmt.of_string fmt s

let rec set_cell_value : type a. a Fmt.t -> a -> Dom_html.element Js.t -> unit =
  fun fmt v cell ->
  match fmt with
  | Option (fmt, default) ->
    begin match v with
      | None -> set_cell_value String default cell
      | Some x -> set_cell_value fmt x cell
    end
  | Custom_elt x ->
    Element.remove_children cell;
    Dom.appendChild cell (x.to_elt v)
  | Html _ ->
    Element.remove_children cell;
    Dom.appendChild cell v
  | fmt ->
    let value = Js.string @@ Fmt.to_string fmt v in
    cell##.textContent := Js.some value

let compare_cells fmt (a : Dom_html.element Js.t as 'a) (b : 'a)=
  Fmt.compare fmt
    (get_cell_value fmt a)
    (get_cell_value fmt b)

let handle_sort (cell : Dom_html.element Js.t) =
  match Element.get_attribute cell "aria-sort" with
  | None -> Dsc
  | Some order ->
    match sort_of_string order with
    | Some Dsc -> Asc
    | _ -> Dsc

module Data = struct
  type _ t =
    | [ ] : unit t
    | (::) : 'a * 'b t -> ('a * 'b) t
end

module Format = struct
  type _ t =
    | [ ] : unit t
    | (::) : (column * 'a Fmt.t) * 'b t -> ('a * 'b) t
end

class ['a] t ~(fmt : 'a Format.t) (elt : Dom_html.element Js.t) = object(self)
  inherit Widget.t elt () as super

  val mutable _fmt : 'a Format.t = fmt

  val mutable row_checkboxes : Checkbox.t array = [||]

  method! init () : unit =
    super#init ()

  method! destroy () : unit =
    super#destroy ()

  method is_empty : bool = match self#rows with [] -> true | _ -> false

  method dense : bool = super#has_class CSS.dense

  method set_dense (x : bool) : unit = super#toggle_class ~force:x CSS.dense

  method rows : Dom_html.element Js.t list =
    Element.query_selector_all super#root Selector.row

  (* Private methods *)

  method private notify_row_selection_changed () : unit =
    ()

  method private notify_selected_all () : unit =
    (* TODO *)
    ()

  method private notify_unselected_all () : unit =
    (* TODO *)
    ()

  method private handle_row_checkbox_change e _ : unit Lwt.t =
    let target = Dom.eventTarget e in
    let row = self#get_row_by_child_element target in
    Js.Opt.case row
      Lwt.return
      (fun _row ->
         (* TODO *)
         Lwt.return_unit)

  method private rows_arr : Dom_html.element Js.t array =
    Element.array_of_node_list
    @@ super#root##querySelectorAll (Js.string Selector.row)

  method private is_checkbox_at_row_index_selected index =
    row_checkboxes.(index)#checked

  method private get_row_by_child_element elt =
    Element.closest elt Selector.row

  method private get_selected_row_count () : int =
    (super#root##querySelectorAll (Js.string Selector.row_selected))##.length

  method private register_row_checkboxes () : unit =
    Array.iter Widget.destroy row_checkboxes;
    let checkboxes =
      Array.map (fun row ->
          Checkbox.attach
          @@ Element.query_selector_exn row Selector.row_checkbox)
      @@ Array.of_list self#rows in
    row_checkboxes <- checkboxes
end
