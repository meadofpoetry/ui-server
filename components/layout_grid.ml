open Widget
open Markup
open Tyxml_js


module Cell = struct

  class t ~content () =

    let elt = Layout_grid.Cell.create ~content:(List.map (fun x -> Of_dom.of_element x#element) content) ()
              |> To_dom.of_div in

    object(self)

      inherit [Dom_html.divElement Js.t] widget elt () as super

      val mutable span         : int option = None
      val mutable span_phone   : int option = None
      val mutable span_tablet  : int option = None
      val mutable span_desktop : int option = None

      val mutable align        : [`Top | `Middle | `Bottom ] option = None
      val mutable order        : int option = None

      method private rm_span ?dt x = super#remove_class @@ Layout_grid.Cell.get_cell_span ?device_type:dt x

      method remove_span         = CCOpt.iter self#rm_span span;                        span         <- None
      method remove_span_phone   = CCOpt.iter (self#rm_span ~dt:`Phone) span_phone;     span_phone   <- None
      method remove_span_tablet  = CCOpt.iter (self#rm_span ~dt:`Tablet) span_tablet;   span_tablet  <- None
      method remove_span_desktop = CCOpt.iter (self#rm_span ~dt:`Desktop) span_desktop; span_desktop <- None

      method remove_align = CCOpt.iter (fun x -> super#remove_class @@ Layout_grid.Cell.get_cell_align x) align;
                            align <- None
      method remove_order = CCOpt.iter (fun x -> super#remove_class @@ Layout_grid.Cell.get_cell_order x) order;
                            order <- None

      method set_span x         = self#remove_span;
                                  super#add_class @@ Layout_grid.Cell.get_cell_span x;
                                  span <- Some x
      method set_span_phone x   = self#remove_span_phone;
                                  super#add_class @@ Layout_grid.Cell.get_cell_span ~device_type:`Phone x;
                                  span_phone <- Some x
      method set_span_tablet x  = self#remove_span_tablet;
                                  super#add_class @@ Layout_grid.Cell.get_cell_span ~device_type:`Tablet x;
                                  span_tablet <- Some x
      method set_span_desktop x = self#remove_span_tablet;
                                  super#add_class @@ Layout_grid.Cell.get_cell_span ~device_type:`Desktop x;
                                  span_tablet <- Some x
      method set_order x        = self#remove_order;
                                  super#add_class @@ Layout_grid.Cell.get_cell_order x;
                                  order <- Some x
      method set_align x        = self#remove_align;
                                  super#add_class @@ Layout_grid.Cell.get_cell_align x;
                                  align <- Some x

      method span         = span
      method span_phone   = span_phone
      method span_tablet  = span_tablet
      method span_desktop = span_desktop
      method align        = align
      method order        = order

    end

end

class t ~(cells:Cell.t list) () =

  let inner = new widget (Layout_grid.create_inner ~cells:(List.map (fun x -> Of_dom.of_div x#element) cells) ()
                          |> To_dom.of_div) () in

  let elt = Layout_grid.create ~content:[ Of_dom.of_div inner#root ] ()
            |> To_dom.of_div in

  object(self)

    inherit [Dom_html.divElement Js.t] widget elt () as super

    val mutable align : [ `Left | `Right ] option = None

    method inner_widget = inner
    method cells        = cells

    method remove_align = CCOpt.iter (fun x -> super#remove_class @@ Layout_grid.get_grid_align x) align;
                          align <- None
    method set_align x  = self#remove_align;
                          super#add_class @@ Layout_grid.get_grid_align x;
                          align <- Some x

    method fixed_column_width     = super#add_class Layout_grid.fixed_column_width_class
    method not_fixed_column_width = super#remove_class Layout_grid.fixed_column_width_class

  end
