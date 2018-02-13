open Containers
open Widget
open Tyxml_js

(* Items *)

module Title = struct

  class t ?large ~title () = object
    inherit widget (Markup.Card.Primary.create_title ?large ~title () |> To_dom.of_h1) () as super

    method set_large x = Markup.Card.Primary.large_title_class
                         |> (fun c -> if x then super#add_class c else super#remove_class c)

    method get_title   = super#get_text_content |> Option.get_or ~default:""
    method set_title s = super#set_text_content s
  end

end

module Subtitle = struct

  class t ~subtitle () = object
    inherit widget (Markup.Card.Primary.create_subtitle ~subtitle () |> To_dom.of_h2) () as super

    method get_subtitle   = super#get_text_content |> Option.get_or ~default:""
    method set_subtitle s = super#set_text_content s
  end

end

module Media_item = struct

  class t ~widget () = object(self)

    val mutable height : [ `Height_1_5 | `Height_2 | `Height_3 ] option = None

    inherit Widget.widget widget () as super

    method get_height    = height
    method remove_height = (match height with
                            | None -> ()
                            | Some `Height_1_5 -> super#remove_class Markup.Card.Media_item.height_1dot5x_class
                            | Some `Height_2   -> super#remove_class Markup.Card.Media_item.height_2x_class
                            | Some `Height_3   -> super#remove_class Markup.Card.Media_item.height_3x_class);
                           height <- None
    method set_height x  = self#remove_height;
                           (match x with
                            | `Height_1_5 -> super#add_class Markup.Card.Media_item.height_1dot5x_class
                            | `Height_2   -> super#add_class Markup.Card.Media_item.height_2x_class
                            | `Height_3   -> super#add_class Markup.Card.Media_item.height_3x_class);
                           height <- Some x

    initializer
      super#add_class Markup.Card.Media_item._class

  end

  class image ~src () =
    let elt = Html.img ~src ~alt:"" () |> To_dom.of_img in

    object(self)
      inherit t ~widget:elt ()

      method image_element : Dom_html.imageElement Js.t = elt
      method get_src   = Js.to_string self#image_element##.src
      method set_src s = self#image_element##.src := Js.string s

    end
end

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

module Primary = struct

  class t ~(widgets:#widget list) () =

    let elt = Markup.Card.Primary.create ~children:(widgets_to_markup widgets) ()
              |> To_dom.of_section in

    object
      val mutable widgets : widget list = List.map (fun x -> (x :> Widget.widget)) widgets
      inherit widget elt ()
      method get_widgets = widgets
    end

end

module Supporting_text = struct

  class t ~text () =
    let elt = Markup.Card.Supporting_text.create ~children:[Html.pcdata text] ()
              |> To_dom.of_section in
    object
      inherit widget elt () as super
      method get_text   = super#get_text_content |> Option.get_or ~default:""
      method set_text s = super#set_text_content s
    end

end

type sections = [ `Actions of Actions.t
                | `Media of Media.t
                | `Primary of Primary.t
                | `Text of Supporting_text.t
                | `Divider ] list

class t ?(form=false) ~(sections:sections) () =
  let tag = if form then Some Tyxml_js.Html.form else None in
  let elt =
    Markup.Card.create ?tag
                       ~sections:(List.map (function
                                            | `Actions x -> widget_to_markup x
                                            | `Media x   -> widget_to_markup x
                                            | `Primary x -> widget_to_markup x
                                            | `Text x    -> widget_to_markup x
                                            | `Divider   -> Markup.List_.Item.create_divider ())
                                           sections)
                       ()
    |> Tyxml_js.To_dom.of_element in

  object(self)
    inherit widget elt ()
    val mutable sections = sections

    method get_sections = sections

    method get_primary = List.find_map (function `Primary x -> Some x | _ -> None) self#get_sections
    method get_actions = List.find_map (function `Actions x -> Some x | _ -> None) self#get_sections
    method get_media   = List.find_map (function `Media x -> Some x   | _ -> None) self#get_sections
    method get_text    = List.find_map (function `Text x -> Some x    | _ -> None) self#get_sections

    method get_all_primary = List.filter_map (function `Primary x -> Some x | _ -> None) self#get_sections
    method get_all_actions = List.filter_map (function `Actions x -> Some x | _ -> None) self#get_sections
    method get_all_media   = List.filter_map (function `Media x -> Some x   | _ -> None) self#get_sections
    method get_all_text    = List.filter_map (function `Text x -> Some x    | _ -> None) self#get_sections
  end
