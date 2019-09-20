open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Layout_grid
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

let ( % ) f g x = f (g x)

module Selector = struct
  let inner = Printf.sprintf ".%s" CSS.inner

  let cell = Printf.sprintf ".%s" CSS.cell
end

let find_map f l =
  let rec aux f = function
    | [] -> None
    | x :: l' -> (
      match f x with
      | Some _ as res -> res
      | None -> aux f l')
  in
  aux f l

let add_nodup ~eq x l = if List.exists (eq x) l then l else x :: l

module Cell = struct
  let parse_align (c : string) =
    let pre = BEM.add_modifier CSS.cell "align" in
    match String.split_on_char '-' c with
    | [prefix; align] when String.equal prefix pre -> align_of_string align
    | _ -> None

  let parse_order (c : string) =
    let pre = BEM.add_modifier CSS.cell "order" in
    match String.split_on_char '-' c with
    | [prefix; order] when String.equal prefix pre -> (
      match int_of_string_opt order with
      | Some x when x > 0 -> Some x
      | _ -> None)
    | _ -> None

  let parse_span (c : string) : (int * device option) option =
    let pre = BEM.add_modifier CSS.cell "span" in
    match String.split_on_char '-' c with
    | [prefix; span] when String.equal prefix pre -> (
      match int_of_string_opt span with
      | Some x when x > 0 && x <= max_columns -> Some (x, None)
      | _ -> None)
    | [prefix; span; device] when String.equal prefix pre -> (
      match int_of_string_opt span with
      | Some x when x > 0 && x <= max_columns -> (
        match device_of_string device with
        | None -> None
        | Some dev -> Some (x, Some dev))
      | _ -> None)
    | _ -> None

  class t (elt : Dom_html.element Js.t) () =
    object
      inherit Widget.t elt () as super

      method span : int option =
        find_map
          (fun (_class : string) ->
            match parse_span _class with
            | Some (x, None) -> Some x
            | _ -> None)
          super#classes

      method span_phone : int option =
        find_map
          (fun (_class : string) ->
            match parse_span _class with
            | Some (x, Some Phone) -> Some x
            | _ -> None)
          super#classes

      method span_tablet : int option =
        find_map
          (fun (_class : string) ->
            match parse_span _class with
            | Some (x, Some Tablet) -> Some x
            | _ -> None)
          super#classes

      method span_desktop : int option =
        find_map
          (fun (_class : string) ->
            match parse_span _class with
            | Some (x, Some Desktop) -> Some x
            | _ -> None)
          super#classes

      method set_span ?(device : device option) (x : int option) : unit =
        List.iter
          (fun (_class : string) ->
            match parse_span _class with
            | Some (_, d) when Option.equal equal_device device d ->
                super#remove_class _class
            | _ -> ())
          super#classes;
        Option.iter (super#add_class % CSS.cell_span ?device) x

      method order : int option = find_map parse_order super#classes

      method set_order (x : int option) : unit =
        List.iter
          (fun (class' : string) ->
            match BEM.get_block class' with
            | Some b when String.equal b CSS.cell_order_prefix ->
                super#remove_class class'
            | _ -> ())
          super#classes;
        Option.iter (super#add_class % CSS.cell_order) x

      method align : cell_align option = find_map parse_align super#classes

      method set_align (x : cell_align option) : unit =
        List.iter
          (fun (class' : string) ->
            match BEM.get_block class' with
            | Some b when String.equal b CSS.cell_align_prefix ->
                super#remove_class class'
            | _ -> ())
          super#classes;
        Option.iter (super#add_class % CSS.cell_align) x
    end

  let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

  let make
      ?classes
      ?a
      ?align
      ?order
      ?span
      ?span_phone
      ?span_tablet
      ?span_desktop
      ?children
      () =
    D.layout_grid_cell
      ?classes
      ?a
      ?align
      ?order
      ?span
      ?span_phone
      ?span_tablet
      ?span_desktop
      ?children
      ()
    |> Tyxml_js.To_dom.of_div
    |> attach
end

class t (elt : Dom_html.element Js.t) () =
  object
    inherit Widget.t elt () as super

    val inner = Element.query_selector_exn elt Selector.inner

    method cells : Dom_html.element Js.t list =
      Element.query_selector_all inner Selector.cell

    method insert_cell_at_idx (i : int) (x : Cell.t) =
      Element.insert_child_at_index inner i x#root

    method append_cell (x : Dom_html.element Js.t) = Dom.appendChild inner x

    method remove_cell (x : Dom_html.element Js.t) =
      try Dom.removeChild inner x with _ -> ()

    method remove_cells () = Element.remove_children inner

    method align : grid_align option =
      if super#has_class (CSS.align Left)
      then Some Left
      else if super#has_class (CSS.align Right)
      then Some Right
      else None

    method set_align (x : grid_align option) : unit =
      List.iter
        (fun (class' : string) ->
          match BEM.get_block class' with
          | Some b when String.equal b CSS.align_prefix -> super#remove_class class'
          | _ -> ())
        super#classes;
      Option.iter (super#add_class % CSS.align) x

    method fixed_column_width : bool = super#has_class CSS.fixed_column_width

    method set_fixed_column_width (x : bool) : unit =
      super#toggle_class ~force:x CSS.fixed_column_width
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()

let make ?classes ?a ?align ?fixed_column_width ?cells ?children () : t =
  D.layout_grid ?classes ?a ?align ?fixed_column_width ?cells ?children ()
  |> Tyxml_js.To_dom.of_div
  |> attach
