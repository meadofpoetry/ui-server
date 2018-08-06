open Containers
open Tyxml_js

module Markup = Components_markup.Tab_scroller.Make(Xml)(Svg)(Html)

type align =
  | Start
  | End
  | Center

class ['a,'b] t ?align ~(tabs:('a,'b) Tab.t list) () =
  let tabs'   = List.map Widget.to_markup tabs in
  let content = Markup.create_scroll_content tabs' () in
  let area    = Markup.create_scroll_area ~content () in
  let elt     = Markup.create ~scroll_area:area ()
                |> To_dom.of_element in
  object(self)

    val mutable _tabs  : ('a,'b) Tab.t list = tabs
    val mutable _align : align option = align

    inherit Widget.t elt ()

    method align : align option = _align
    method set_align (x:align option) : unit =
      _align <- x;
      let pre = Components_markup.CSS.add_modifier
                  Markup.base_class "align-" in
      List.iter self#remove_class @@ self#find_classes pre;
      match x with
      | None -> ()
      | Some Start  -> self#add_class Markup.align_start_class
      | Some End    -> self#add_class Markup.align_end_class
      | Some Center -> self#add_class Markup.align_center_class

    method tabs = _tabs

    initializer
      self#set_align _align

  end
