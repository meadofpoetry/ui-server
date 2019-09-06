open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Layout_grid
module Markup = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let ( % ) f g x = f (g x)

module Selector = struct
  let inner = Printf.sprintf ".%s" CSS.inner
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

  class t ?widgets (elt : Dom_html.element Js.t) () =
    object
      inherit Widget.t elt () as super

      method! layout () : unit =
        (match widgets with
        | None -> ()
        | Some x -> List.iter Widget.layout x);
        super#layout ()

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

  let make
      ?span
      ?span_phone
      ?span_tablet
      ?span_desktop
      ?align
      ?order
      (widgets : #Widget.t list) : t =
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_element
      @@ Markup.create_cell
           ?span
           ?span_phone
           ?span_tablet
           ?span_desktop
           ?align
           ?order
           (List.map Widget.to_markup widgets)
           ()
    in
    new t ~widgets elt ()

  let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()
end

class t (elt : Dom_html.element Js.t) () =
  let inner = Element.query_selector_exn elt Selector.inner in
  let cells = List.map Cell.attach @@ Element.children inner in
  object
    inherit Widget.t elt () as super

    val mutable _cells = cells

    method! layout () : unit =
      List.iter Widget.layout _cells;
      super#layout ()

    method cells : Cell.t list = _cells

    method insert_cell_at_idx (i : int) (x : Cell.t) =
      _cells <- add_nodup ~eq:Widget.equal x _cells;
      Element.insert_child_at_index inner i x#root

    method append_cell (x : Cell.t) =
      _cells <- add_nodup ~eq:Widget.equal x _cells;
      Dom.appendChild inner x#root

    method remove_cell (x : Cell.t) =
      _cells <- List.filter (fun cell -> not @@ Widget.equal cell x) _cells;
      try Dom.removeChild inner x#root with _ -> ()

    method remove_cells () =
      _cells <- [];
      Element.remove_children inner

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

let make ?align ?fixed_column_width (cells : Cell.t list) : t =
  let inner = Markup.create_inner ~cells:(List.map Widget.to_markup cells) () in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element @@ Markup.create ?align ?fixed_column_width ~inner ()
  in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t = new t (Element.coerce elt) ()
