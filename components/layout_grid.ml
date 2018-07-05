open Containers
open Tyxml_js

module Markup = Components_markup.Layout_grid.Make(Xml)(Svg)(Html)

module Cell = struct

  class t ~(widgets:#Widget.t list) () =

    let elt = Markup.Cell.create ~content:(List.map Widget.to_markup widgets) ()
              |> Tyxml_js.To_dom.of_div in

    object(self)

      inherit Widget.t elt () as super

      val mutable widgets : Widget.t list = List.map (fun x -> (x :> Widget.t)) widgets

      val mutable span         : int option = None
      val mutable span_phone   : int option = None
      val mutable span_tablet  : int option = None
      val mutable span_desktop : int option = None

      val mutable align        : [`Top | `Middle | `Bottom ] option = None
      val mutable order        : int option = None

      method private rm_span ?dt x = super#remove_class @@ Markup.Cell.get_cell_span ?device_type:dt x

      method remove_span =
        Option.iter self#rm_span span;
        span <- None
      method span       = span
      method set_span x =
        self#remove_span;
        super#add_class @@ Markup.Cell.get_cell_span x;
        span <- Some x

      method span_phone       = span_phone
      method set_span_phone : int option -> unit = function
        | Some x -> Option.iter (self#rm_span ~dt:`Phone) span_phone;
                    super#add_class @@ Markup.Cell.get_cell_span ~device_type:`Phone x;
                    span_phone <- Some x
        | None   -> Option.iter (self#rm_span ~dt:`Phone) span_phone;
                    span_phone <- None

      method span_tablet = span_tablet
      method set_span_tablet : int option -> unit = function
        | Some x -> Option.iter (self#rm_span ~dt:`Tablet) span_tablet;
                    super#add_class @@ Markup.Cell.get_cell_span ~device_type:`Tablet x;
                    span_tablet <- Some x
        | None   -> Option.iter (self#rm_span ~dt:`Tablet) span_tablet;
                    span_tablet <- None

      method remove_span_desktop =
        Option.iter (self#rm_span ~dt:`Desktop) span_desktop;
        span_desktop <- None
      method span_desktop = span_desktop
      method set_span_desktop : int option -> unit = function
        | Some x -> Option.iter (self#rm_span ~dt:`Desktop) span_desktop;
                    super#add_class @@ Markup.Cell.get_cell_span ~device_type:`Desktop x;
                    span_tablet <- Some x
        | None   -> Option.iter (self#rm_span ~dt:`Desktop) span_desktop;
                    span_desktop <- None

      method order = order
      method set_order : int option -> unit = function
        | Some x -> Option.iter (fun x -> super#remove_class @@ Markup.Cell.get_cell_order x) order;
                    super#add_class @@ Markup.Cell.get_cell_order x;
                    order <- Some x
        | None   -> Option.iter (fun x -> super#remove_class @@ Markup.Cell.get_cell_order x) order;
                    order <- None

      method align = align
      method set_align : [`Top | `Middle | `Bottom ] option -> unit = function
        | Some x -> Option.iter (fun x -> super#remove_class @@ Markup.Cell.get_cell_align x) align;
                    super#add_class @@ Markup.Cell.get_cell_align x;
                    align <- Some x
        | None   -> Option.iter (fun x -> super#remove_class @@ Markup.Cell.get_cell_align x) align;
                    align <- None

      method widgets = widgets

    end

end

class t ~(cells:Cell.t list) () =

  let inner = new Widget.t (Markup.create_inner ~cells:(List.map Widget.to_markup cells) ()
                                 |> Tyxml_js.To_dom.of_div) () in
  let elt   = Markup.create ~content:[Widget.to_markup inner] ()
              |> Tyxml_js.To_dom.of_div in

  object
    inherit Widget.t elt () as super

    val mutable align : [ `Left | `Right ] option = None

    method inner = inner
    method cells = cells

    method align       = align
    method set_align : [ `Left | `Right ] option -> unit = function
      | Some x -> Option.iter (fun x -> super#remove_class @@ Markup.get_grid_align x) align;
                  super#add_class @@ Markup.get_grid_align x;
                  align <- Some x
      | None   -> Option.iter (fun x -> super#remove_class @@ Markup.get_grid_align x) align;
                  align <- None

    method set_fixed_column_width x = Markup.fixed_column_width_class
                                      |> (fun c -> if x then super#add_class c else super#remove_class c)
  end
