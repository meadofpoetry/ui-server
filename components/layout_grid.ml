open Containers
open Tyxml_js

module Markup = Components_markup.Layout_grid.Make(Xml)(Svg)(Html)

module Cell = struct

  class t ?span ?span_phone ?span_tablet ?span_desktop
          ~(widgets:#Widget.t list) () =

    let elt =
      Markup.Cell.create ~content:(List.map Widget.to_markup widgets) ()
      |> Tyxml_js.To_dom.of_div in

    object(self)

      inherit Widget.t elt () as super

      val mutable widgets : Widget.t list =
        List.map (fun x -> (x :> Widget.t)) widgets

      val mutable _span         : int option = span
      val mutable _span_phone   : int option = span_phone
      val mutable _span_tablet  : int option = span_tablet
      val mutable _span_desktop : int option = span_desktop

      val mutable align        : [`Top | `Middle | `Bottom ] option = None
      val mutable order        : int option = None

      method private rm_span ?dt x =
        super#remove_class @@ Markup.Cell.get_cell_span ?device_type:dt x

      method span = _span
      method set_span = function
        | Some x -> Option.iter self#rm_span _span;
                    super#add_class @@ Markup.Cell.get_cell_span x;
                    _span <- Some x
        | None   -> Option.iter self#rm_span _span;
                    _span <- None

      method span_phone = _span_phone
      method set_span_phone : int option -> unit = function
        | Some x -> Option.iter (self#rm_span ~dt:`Phone) _span_phone;
                    super#add_class @@ Markup.Cell.get_cell_span ~device_type:`Phone x;
                    _span_phone <- Some x
        | None   -> Option.iter (self#rm_span ~dt:`Phone) _span_phone;
                    _span_phone <- None

      method span_tablet = _span_tablet
      method set_span_tablet : int option -> unit = function
        | Some x -> Option.iter (self#rm_span ~dt:`Tablet) _span_tablet;
                    super#add_class @@ Markup.Cell.get_cell_span ~device_type:`Tablet x;
                    _span_tablet <- Some x
        | None   -> Option.iter (self#rm_span ~dt:`Tablet) _span_tablet;
                    _span_tablet <- None

      method span_desktop = _span_desktop
      method set_span_desktop : int option -> unit = function
        | Some x -> Option.iter (self#rm_span ~dt:`Desktop) _span_desktop;
                    super#add_class @@ Markup.Cell.get_cell_span ~device_type:`Desktop x;
                    _span_tablet <- Some x
        | None   -> Option.iter (self#rm_span ~dt:`Desktop) _span_desktop;
                    _span_desktop <- None

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

      initializer
        self#set_span _span;
        self#set_span_phone _span_phone;
        self#set_span_tablet _span_tablet;
        self#set_span_desktop _span_desktop;

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
