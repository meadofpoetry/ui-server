open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

include Components_tyxml.Layout_grid
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Cell = struct

  let parse_align (c : string) =
    let pre = Components_tyxml.BEM.add_modifier CSS.cell "align" in
    match String.split_on_char '-' c with
    | [prefix; align] when String.equal prefix pre ->
       align_of_string align
    | _ -> None

  let parse_order (c : string) =
    let pre = Components_tyxml.BEM.add_modifier CSS.cell "order" in
    match String.split_on_char '-' c with
    | [prefix; order] when String.equal prefix pre ->
       begin match int_of_string_opt order with
       | Some x when x > 0 -> Some x
       | _ -> None
       end
    | _ -> None

  let parse_span (c : string) : (int * device option) option =
    let pre = Components_tyxml.BEM.add_modifier CSS.cell "span" in
    match String.split_on_char '-' c with
    | [prefix; span] when String.equal prefix pre ->
       begin match int_of_string_opt span with
       | Some x when x > 0 && x <= max_columns -> Some (x, None)
       | _ -> None
       end
    | [prefix; span; device] when String.equal prefix pre ->
       begin match int_of_string_opt span with
       | Some x when x > 0 && x <= max_columns ->
          begin match device_of_string device with
          | None -> None
          | Some dev -> Some (x, Some dev)
          end
       | _ -> None
       end
    | _ -> None

  class t (elt : Dom_html.element Js.t) () =
  object
    inherit Widget.t elt () as super

    method span : int option =
      List.find_map (fun (_class : string) ->
          match parse_span _class with
          | Some (x, None) -> Some x
          | _ -> None) super#classes

    method span_phone : int option =
      List.find_map (fun (_class : string) ->
          match parse_span _class with
          | Some (x, Some Phone) -> Some x
          | _ -> None) super#classes

    method span_tablet : int option =
      List.find_map (fun (_class : string) ->
          match parse_span _class with
          | Some (x, Some Tablet) -> Some x
          | _ -> None) super#classes

    method span_desktop : int option =
      List.find_map (fun (_class : string) ->
          match parse_span _class with
          | Some (x, Some Desktop) -> Some x
          | _ -> None) super#classes

    method set_span ?(device : device option) (x : int option) : unit =
      List.iter (fun (_class : string) ->
          match parse_span _class with
          | Some (_, d) when Option.equal ~eq:equal_device device d ->
             super#remove_class _class
          | _ -> ()) super#classes;
      Option.iter (super#add_class % CSS.cell_span ?device) x

    method order : int option =
      List.find_map parse_order super#classes

    method set_order (x : int option) : unit =
      List.iter (fun (_class : string) ->
          if String.prefix ~pre:CSS.cell_order_prefix _class
          then super#remove_class _class) super#classes;
      Option.iter (super#add_class % CSS.cell_order) x

    method align : cell_align option =
      List.find_map parse_align super#classes

    method set_align (x : cell_align option) : unit =
      List.iter (fun (_class : string) ->
          if String.prefix ~pre:CSS.cell_align_prefix _class
          then super#remove_class _class) super#classes;
      Option.iter (super#add_class % CSS.cell_align) x
  end

  let make ?span ?span_phone ?span_tablet ?span_desktop ?align ?order
        (widgets : #Widget.t list) : t =
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_element
      @@ Markup.create_cell ?span ?span_phone ?span_tablet ?span_desktop
           ?align ?order (List.map Widget.to_markup widgets) () in
    new t elt ()

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Element.coerce elt) ()
end

class t (elt : Dom_html.element Js.t) () =
  let inner = find_element_by_class_exn elt CSS.inner in
  let cells =
    List.map Cell.attach
    @@ Element.children inner in
  object
    inherit Widget.t elt () as super

    val mutable _cells = cells

    method cells : Cell.t list = _cells

    method insert_cell_at_idx (i : int) (x : Cell.t) =
      _cells <- List.add_nodup ~eq:Widget.equal x _cells;
      Element.insert_child_at_index inner i x#root

    method append_cell (x : Cell.t) =
      _cells <- List.add_nodup ~eq:Widget.equal x _cells;
      Dom.appendChild inner x#root

    method remove_cell (x : Cell.t) =
      _cells <- List.remove ~eq:Widget.equal x _cells;
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
      List.iter (fun (_class : string) ->
          if String.prefix ~pre:CSS.align_prefix _class
          then super#remove_class _class) super#classes;
      Option.iter (super#add_class % CSS.align) x

    method fixed_column_width : bool =
      super#has_class CSS.fixed_column_width

    method set_fixed_column_width (x : bool) : unit =
      super#toggle_class ~force:x CSS.fixed_column_width
  end

let make ?align ?fixed_column_width (cells : Cell.t list) : t =
  let inner = Markup.create_inner ~cells:(List.map Widget.to_markup cells) () in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?align ?fixed_column_width ~inner () in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
