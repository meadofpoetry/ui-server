open Containers
open Widget
open Tyxml_js

(* Sections *)

module Actions = struct

  module Buttons = struct

  end

  module Icons = struct

  end

  class t ~(widgets:#widget list) () =
    let elt = Markup.Card.Actions.create ~children:(widgets_to_markup widgets) ()
              |> To_dom.of_section in
    object
      val mutable widgets : widget list = List.map (fun x -> (x :> Widget.widget)) widgets
      inherit widget elt () as super
      method get_widgets = widgets

      initializer
        List.iter (fun x -> x#add_class Markup.Card.Actions.action_class) widgets
    end

end

module Media = struct

  class t ~(widgets:#widget list) () =
    let elt = Markup.Card.Media.create ~children:(widgets_to_markup widgets) ()
              |> To_dom.of_section in
    object
      val mutable widgets : widget list = List.map (fun x -> (x :> Widget.widget)) widgets
      inherit widget elt ()
      method get_widgets = widgets
    end

end

class t ?(form=false) ~(widgets:#Widget.widget list) () =
  let tag = if form then Some Tyxml_js.Html.form else None in
  let elt =
    Markup.Card.create ?tag ~sections:(List.map Widget.widget_to_markup widgets) ()
    |> Tyxml_js.To_dom.of_element in

  object(self)
    inherit widget elt ()
  end
