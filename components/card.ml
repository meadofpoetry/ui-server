open Containers
open Widget
open Tyxml_js

(* Sections *)

module Actions = struct

  class t ?vertical ~(widgets:#widget list) () =
    let elt = Markup.Card.Actions.create ?vertical ~children:(widgets_to_markup widgets) ()
              |> To_dom.of_section in
    object
      val mutable widgets : widget list = List.map (fun x -> (x :> Widget.widget)) widgets
      inherit widget elt () as super
      method get_widgets = widgets
      method set_vertical x = Markup.Card.Actions.vertical_class
                              |> (fun c -> if x then super#add_class c else super#remove_class c)

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
