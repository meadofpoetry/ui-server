open Containers
open Tyxml_js

module Markup = Components_tyxml.Box.Make(Xml)(Svg)(Html)

type direction =
  [ `Row
  | `Column
  ]

class t ?tag ?(gap = 0)
        ?(justify_content = `Start)
        ?(align_items = `Stretch)
        ?(align_content = `Stretch)
        ?(wrap = `Nowrap)
        ~direction
        ~(widgets : #Widget.t list)
        () =
  let vertical = match direction with
    | `Row -> false
    | `Column -> true in
  let content = List.map Widget.to_markup widgets in
  let elt = To_dom.of_element @@ Markup.create ?tag ~vertical ~content () in
  object(self)

    val mutable _justify_content : Markup.justify_content = justify_content
    val mutable _align_items : Markup.align_items = align_items
    val mutable _align_content : Markup.align_content = align_content
    val mutable _wrap : Markup.wrap = wrap
    val mutable _gap : int = gap

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      _widgets <- List.map Widget.coerce widgets;
      self#set_wrap _wrap;
      self#set_justify_content _justify_content;
      self#set_align_items _align_items;
      self#set_align_content _align_content;
      self#set_gap gap

    method set_direction : direction -> unit = function
      | `Row ->
         self#remove_class Markup.vertical_class;
         self#add_class Markup.horizontal_class
      | `Column ->
         self#remove_class Markup.horizontal_class;
         self#add_class Markup.vertical_class

    method gap = _gap
    method set_gap (x : int) : unit =
      _gap <- x (* TODO implement *)

    method wrap = _wrap
    method set_wrap (x : Markup.wrap) : unit =
      self#remove_wrap;
      self#add_class @@ Markup.get_wrap_class x;
      _wrap <-x

    method justify_content = justify_content
    method set_justify_content x =
      self#remove_justify_content;
      self#add_class @@ Markup.get_justify_content_class x;
      _justify_content <- x

    method align_items = align_items
    method set_align_items x =
      self#remove_align_items;
      self#add_class @@ Markup.get_align_items_class x;
      _align_items <- x

    method align_content = align_content
    method set_align_content x =
      self#remove_align_content;
      self#add_class @@ Markup.get_align_content_class x;
      _align_content <- x

    method remove_wrap =
      self#remove_class @@ Markup.get_wrap_class _wrap
    method private remove_justify_content =
      List.iter self#remove_class
      @@ self#find_classes Markup.justify_content_class_prefix
    method private remove_align_items =
      List.iter self#remove_class
      @@ self#find_classes Markup.align_items_class_prefix
    method private remove_align_content =
      List.iter self#remove_class
      @@ self#find_classes Markup.align_content_class_prefix

  end
