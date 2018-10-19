open Containers
open Tyxml_js

module Markup = Components_markup.Card.Make(Xml)(Svg)(Html)

(* Sections *)

module Actions = struct

  module Buttons = struct

    class t ~(widgets : #Widget.t list) () =
      let children = List.map (fun x -> Widget.to_markup x#widget) widgets in
      let elt =
        Markup.Actions.Buttons.create ~children ()
        |> To_dom.of_element in
      object
        inherit Widget.t elt ()

        initializer
          List.iter (fun x ->
              x#add_class Markup.Actions.action_class;
              x#add_class Markup.Actions.action_button_class)
            widgets

      end

  end

  module Icons = struct

    class t ~(widgets : #Widget.t list) () =
      let children = List.map (fun x -> Widget.to_markup x#widget) widgets in
      let elt =
        Markup.Actions.Icons.create ~children ()
        |> To_dom.of_element in
      object
        inherit Widget.t elt ()

        initializer
          List.iter (fun x ->
              x#add_class Markup.Actions.action_class;
              x#add_class Markup.Actions.action_icon_class)
            widgets

      end

  end

  class t ~(widgets : #Widget.t list) () =
    let elt =
      Markup.Actions.create ~children:(List.map Widget.to_markup widgets) ()
      |> To_dom.of_section in
    object
      val mutable widgets : Widget.t list =
        List.map (fun x -> (x :> Widget.t)) widgets

      inherit Widget.t elt () as super

      method widgets = widgets

    end

end

module Primary = struct

  class overline text () =
    let elt =
      Markup.Primary.create_overline ~text ()
      |> To_dom.of_element in
    object
      inherit Widget.t elt ()
    end

  class title ?large text () =
    let elt =
      Markup.Primary.create_title ?large ~title:text ()
      |> To_dom.of_element in
    object
      inherit Widget.t elt ()
    end

  class subtitle text () =
    let elt =
      Markup.Primary.create_subtitle ~subtitle:text ()
      |> To_dom.of_element in
    object
      inherit Widget.t elt ()
    end

  class t ~(widgets:#Widget.t list) () =
    let elt =
      Markup.Primary.create ~children:(List.map Widget.to_markup widgets) ()
      |> To_dom.of_element in
    object
      inherit Widget.t elt ()
    end

end

module Media = struct

  class t ~(widgets : #Widget.t list) () =
    let elt =
      Markup.Media.create ~children:(List.map Widget.to_markup widgets) ()
      |> To_dom.of_section in
    object
      val mutable widgets : Widget.t list =
        List.map (fun x -> (x :> Widget.t)) widgets

      inherit Widget.t elt ()

      method widgets = widgets
    end

end

class t ?(outlined = false)
        ?(form = false)
        ~(widgets : #Widget.t list)
        () =
  let tag = if form then Some Html.form else None in
  let elt =
    Markup.create ?tag ~sections:(List.map Widget.to_markup widgets) ()
    |> To_dom.of_element in

  object(self)
    inherit Widget.t elt ()

    method init () : unit =
      self#set_outlined outlined

    method outlined : bool =
      self#has_class Markup.outlined_class

    method set_outlined (x : bool) : unit =
      self#add_or_remove_class x Markup.outlined_class

  end
