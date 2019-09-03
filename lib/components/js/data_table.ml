open Js_of_ocaml
open Js_of_ocaml_tyxml

include Components_tyxml.Data_table

module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Fmt = Markup.Fmt

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

  method! init () : unit =
    super#init ()

  method! destroy () : unit =
    super#destroy ()

  method is_empty : bool = match self#rows with [] -> true | _ -> false

  method dense : bool = super#has_class CSS.dense

  method set_dense (x : bool) : unit = super#toggle_class ~force:x CSS.dense

  method rows : Dom_html.element Js.t list = []
end
