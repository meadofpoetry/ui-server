open Containers
open Tyxml_js

module Markup = Components_markup.Box.Make(Xml)(Svg)(Html)

type justify_content = [ `Start | `End | `Center | `Space_between | `Space_around | `Space_evenly ]
type align_items     = [ `Start | `End | `Center | `Stretch | `Baseline ]
type align_content   = [ `Start | `End | `Center | `Stretch | `Space_between | `Space_around ]

class t ?(vertical=true) ?tag ?(gap=0) ~(widgets:#Widget.t list) () =
  let elt = Markup.create ~vertical ~content:(List.map Widget.to_markup widgets) ?tag ()
            |> Tyxml_js.To_dom.of_element in
  object(self)

    val mutable widgets : Widget.t list = List.map (fun x -> Widget.coerce x) widgets
    val mutable justify_content : justify_content option = None
    val mutable align_items     : align_items option     = None
    val mutable align_content   : align_content option   = None
    val mutable gap             : int                    = gap

    inherit Widget.t elt () as super

    method widgets        = widgets
    method set_vertical   = super#remove_class Markup.horizontal_class;
                            super#add_class Markup.vertical_class
    method set_horizontal = super#remove_class Markup.vertical_class;
                            super#add_class Markup.horizontal_class

    method set_gap x : unit =
      gap <- x;
      (Js.Unsafe.coerce self#root##.style)##setProperty
        (Js.string "--mdc-box-margin")
        (Js.string @@ Printf.sprintf "%dpx" x)
    method gap = gap

    method justify_content = justify_content
    method remove_justify_content =
      Option.iter (fun x -> super#remove_class @@ Markup.get_justify_content_class x) justify_content;
      justify_content <- None
    method set_justify_content x =
      self#remove_justify_content;
      super#add_class @@ Markup.get_justify_content_class x;
      justify_content <- Some x

    method align_items = align_items
    method remove_align_items =
      Option.iter (fun x -> super#remove_class @@ Markup.get_align_items_class x) align_items;
      align_items <- None
    method set_align_items x =
      self#remove_align_items;
      super#add_class @@ Markup.get_align_items_class x;
      align_items <- Some x

    method align_content = align_content
    method remove_align_content =
      Option.iter (fun x -> super#remove_class @@ Markup.get_align_content_class x) align_content;
      align_content <- None
    method set_align_content x =
      self#remove_align_content;
      super#add_class @@ Markup.get_align_content_class x;
      align_content <- Some x

    initializer
      self#set_gap gap

  end
