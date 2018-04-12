open Containers

module Cell = struct

  class t ~(widgets:#Widget.widget list) () =

    let elt = Markup.Layout_grid.Cell.create ~content:(Widget.widgets_to_markup widgets) ()
              |> Tyxml_js.To_dom.of_div in

    object(self)

      inherit Widget.widget elt () as super

      val mutable widgets : Widget.widget list = List.map (fun x -> (x :> Widget.widget)) widgets

      val mutable span         : int option = None
      val mutable span_phone   : int option = None
      val mutable span_tablet  : int option = None
      val mutable span_desktop : int option = None

      val mutable align        : [`Top | `Middle | `Bottom ] option = None
      val mutable order        : int option = None

      method private rm_span ?dt x = super#remove_class @@ Markup.Layout_grid.Cell.get_cell_span ?device_type:dt x

      method remove_span =
        Option.iter self#rm_span span;
        span <- None
      method get_span = span
      method set_span x =
        self#remove_span;
        super#add_class @@ Markup.Layout_grid.Cell.get_cell_span x;
        span <- Some x

      method remove_span_phone =
        Option.iter (self#rm_span ~dt:`Phone) span_phone;
        span_phone <- None
      method get_span_phone = span_phone
      method set_span_phone x =
        self#remove_span_phone;
        super#add_class @@ Markup.Layout_grid.Cell.get_cell_span ~device_type:`Phone x;
        span_phone <- Some x

      method remove_span_tablet =
        Option.iter (self#rm_span ~dt:`Tablet) span_tablet;
        span_tablet <- None
      method get_span_tablet = span_tablet
      method set_span_tablet x =
        self#remove_span_tablet;
        super#add_class @@ Markup.Layout_grid.Cell.get_cell_span ~device_type:`Tablet x;
        span_tablet <- Some x

      method remove_span_desktop =
        Option.iter (self#rm_span ~dt:`Desktop) span_desktop;
        span_desktop <- None
      method get_span_desktop = span_desktop
      method set_span_desktop x =
        self#remove_span_tablet;
        super#add_class @@ Markup.Layout_grid.Cell.get_cell_span ~device_type:`Desktop x;
        span_tablet <- Some x

      method remove_order =
        Option.iter (fun x -> super#remove_class @@ Markup.Layout_grid.Cell.get_cell_order x) order;
        order <- None
      method get_order = order
      method set_order x =
        self#remove_order;
        super#add_class @@ Markup.Layout_grid.Cell.get_cell_order x;
        order <- Some x

      method remove_align =
        Option.iter (fun x -> super#remove_class @@ Markup.Layout_grid.Cell.get_cell_align x) align;
        align <- None
      method get_align = align
      method set_align x =
        self#remove_align;
        super#add_class @@ Markup.Layout_grid.Cell.get_cell_align x;
        align <- Some x

      method get_widgets = widgets

    end

end

class t ~(cells:Cell.t list) () =

  let inner = new Widget.widget (Markup.Layout_grid.create_inner ~cells:(Widget.widgets_to_markup cells) ()
                                 |> Tyxml_js.To_dom.of_div) () in
  let elt   = Markup.Layout_grid.create ~content:[Widget.widget_to_markup inner] ()
              |> Tyxml_js.To_dom.of_div in

  object(self)
    inherit Widget.widget elt () as super

    val mutable align : [ `Left | `Right ] option = None

    method inner = inner
    method cells = cells

    method remove_align =
      Option.iter (fun x -> super#remove_class @@ Markup.Layout_grid.get_grid_align x) align;
      align <- None
    method align       = align
    method set_align x =
      self#remove_align;
      super#add_class @@ Markup.Layout_grid.get_grid_align x;
      align <- Some x

    method set_fixed_column_width x = Markup.Layout_grid.fixed_column_width_class
                                      |> (fun c -> if x then super#add_class c else super#remove_class c)
  end
