open Js_of_ocaml
open Containers
open Tyxml_js
open Fun

module Markup = Components_tyxml.Layout_grid.Make(Xml)(Svg)(Html)

module Cell = struct

  open Markup.Cell

  let iter = Option.iter

  class t ?span ?span_phone ?span_tablet ?span_desktop
          ~(widgets : #Widget.t list)
          () =

    let (elt : Dom_html.element Js.t) =
      create ~content:(List.map Widget.to_markup widgets) ()
      |> To_dom.of_div in

    object(self)

      inherit Widget.t elt () as super

      val mutable _span : int option = span
      val mutable _span_phone : int option = span_phone
      val mutable _span_tablet : int option = span_tablet
      val mutable _span_desktop : int option = span_desktop

      val mutable align : [`Top | `Middle | `Bottom ] option = None
      val mutable order : int option = None

      method! init () : unit =
        super#init ();
        self#set_span _span;
        self#set_span_phone _span_phone;
        self#set_span_tablet _span_tablet;
        self#set_span_desktop _span_desktop;

      method! layout () : unit =
        super#layout ();

      method span = _span
      method set_span = function
        | Some x ->
           iter self#rm_span _span;
           super#add_class @@ get_cell_span x;
           _span <- Some x
        | None   ->
           iter self#rm_span _span;
           _span <- None

      method span_phone = _span_phone
      method set_span_phone : int option -> unit = function
        | Some x ->
           iter (self#rm_span ~dt:`Phone) _span_phone;
           super#add_class @@ get_cell_span ~device_type:`Phone x;
           _span_phone <- Some x
        | None   ->
           iter (self#rm_span ~dt:`Phone) _span_phone;
           _span_phone <- None

      method span_tablet = _span_tablet
      method set_span_tablet : int option -> unit = function
        | Some x ->
           iter (self#rm_span ~dt:`Tablet) _span_tablet;
           super#add_class @@ get_cell_span ~device_type:`Tablet x;
           _span_tablet <- Some x
        | None   ->
           iter (self#rm_span ~dt:`Tablet) _span_tablet;
           _span_tablet <- None

      method span_desktop = _span_desktop
      method set_span_desktop : int option -> unit = function
        | Some x ->
           iter (self#rm_span ~dt:`Desktop) _span_desktop;
           super#add_class @@ get_cell_span ~device_type:`Desktop x;
           _span_tablet <- Some x
        | None   ->
           iter (self#rm_span ~dt:`Desktop) _span_desktop;
           _span_desktop <- None

      method order = order
      method set_order : int option -> unit = function
        | Some x ->
           iter (super#remove_class % get_cell_order) order;
           super#add_class @@ get_cell_order x;
           order <- Some x
        | None   ->
           iter (super#remove_class % get_cell_order) order;
           order <- None

      method align = align
      method set_align : [`Top | `Middle | `Bottom ] option -> unit = function
        | Some x ->
           iter (super#remove_class % get_cell_align) align;
           super#add_class @@ get_cell_align x;
           align <- Some x
        | None   ->
           iter (super#remove_class % get_cell_align) align;
           align <- None

      method private rm_span ?dt x =
        super#remove_class @@ get_cell_span ?device_type:dt x

    end

end

class t ?align ~(cells : Cell.t list) () =
  let inner =
    Markup.create_inner ~cells:(List.map Widget.to_markup cells) ()
    |> To_dom.of_div
    |> Widget.create in
  let (elt : Dom_html.element Js.t) =
    Markup.create ~content:[Widget.to_markup inner] ()
    |> To_dom.of_div in

  object(self)
    inherit Widget.t elt () as super

    val mutable _cells = cells

    val mutable _align : [ `Left | `Right ] option = align

    method! layout () : unit =
      super#layout ();
      inner#layout ();
      List.iter (fun x -> x#layout ()) _cells

    method inner = inner
    method cells = _cells

    method insert_cell_at_idx (i : int) (x : Cell.t) =
      _cells <- List.add_nodup ~eq:Widget.equal x _cells;
      self#inner#insert_child_at_idx i x

    method append_cell (x : Cell.t) =
      _cells <- List.add_nodup ~eq:Widget.equal x _cells;
      self#inner#append_child x

    method remove_cell (x : Cell.t) =
      _cells <- List.remove ~eq:Widget.equal x _cells;
      self#inner#remove_child x

    method align = _align
    method set_align : [ `Left | `Right ] option -> unit = function
      | Some x ->
         Option.iter (super#remove_class % Markup.get_grid_align) _align;
         super#add_class @@ Markup.get_grid_align x;
         _align <- Some x
      | None   ->
         Option.iter (super#remove_class % Markup.get_grid_align) _align;
         _align <- None

    method set_fixed_column_width x =
      super#toggle_class ~force:x Markup.fixed_column_width_class

  end
