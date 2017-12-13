type justify_content = [ `Start | `End | `Center | `Space_between | `Space_around | `Space_evenly ]
type align_items     = [ `Start | `End | `Center | `Stretch | `Baseline ]
type align_content   = [ `Start | `End | `Center | `Stretch | `Space_between | `Space_around ]

class t ?(vertical=true) ?tag ~(widgets:#Widget.widget list) () =
  let elt = Markup.Box.create ~vertical ~content:(Widget.widgets_to_markup widgets) ?tag ()
            |> Tyxml_js.To_dom.of_element in
  object(self)

    val mutable widgets : Widget.widget list = List.map (fun x -> Widget.coerce x) widgets
    val mutable justify_content : justify_content option = None
    val mutable align_items     : align_items option     = None
    val mutable align_content   : align_content option   = None

    inherit Widget.widget elt () as super

    method get_widgets    = widgets
    method set_vertical   = super#remove_class Markup.Box.horizontal_class;
                            super#add_class Markup.Box.vertical_class
    method set_horizontal = super#remove_class Markup.Box.vertical_class;
                            super#add_class Markup.Box.horizontal_class

    method get_justify_content = justify_content
    method remove_justify_content =
      CCOpt.iter (fun x -> super#remove_class @@ Markup.Box.get_justify_content_class x) justify_content;
      justify_content <- None
    method set_justify_content x =
      self#remove_justify_content;
      super#add_class @@ Markup.Box.get_justify_content_class x;
      justify_content <- Some x

    method get_align_items = align_items
    method remove_align_items =
      CCOpt.iter (fun x -> super#remove_class @@ Markup.Box.get_align_items_class x) align_items;
      align_items <- None
    method set_align_items x =
      self#remove_align_items;
      super#add_class @@ Markup.Box.get_align_items_class x;
      align_items <- Some x

    method get_align_content = align_content
    method remove_align_content =
      CCOpt.iter (fun x -> super#remove_class @@ Markup.Box.get_align_content_class x) align_content;
      align_content <- None
    method set_align_content x =
      self#remove_align_content;
      super#add_class @@ Markup.Box.get_align_content_class x;
      align_content <- Some x

  end
