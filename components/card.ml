open Widget
open Tyxml_js

module Media_item = struct
  class ['a] t ~widget () = object(self)

    val mutable height : [ `Height_1_5 | `Height_2 | `Height_3 ] option = None

    inherit ['a] Widget.widget widget () as super

    method height        = height
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

    object
      inherit [Dom_html.imageElement Js.t] t ~widget:elt () as super

      method src       = Js.to_string super#root##.src
      method set_src s = super#root##.src := Js.string s

    end
end

module Actions = struct

  class t ?vertical ~children () =
    let elt = Markup.Card.Actions.create ?vertical
                                         ~children:(List.map (fun x -> Of_dom.of_element x#element) children)
                                         ()
              |> To_dom.of_section in

    object

      inherit [Dom_html.element Js.t] widget elt () as super

      method vertical   = super#add_class Markup.Card.Actions.vertical_class
      method horizontal = super#remove_class Markup.Card.Actions.vertical_class

      initializer
        List.iter (fun x -> x#add_class Markup.Card.Actions.action_class) children

    end

end

module Media = struct

  class t ~children () =
    let elt = Markup.Card.Media.create ~children:(List.map (fun x -> Of_dom.of_element x#element) children) ()
              |> To_dom.of_section in
    object
      inherit [Dom_html.element Js.t] widget elt ()
    end

end

module Primary = struct

  module Title = struct

    class t ?large ~title () = object
      inherit [Dom_html.headingElement Js.t] widget (Markup.Card.Primary.create_title ?large ~title ()
                                                     |> To_dom.of_h1) () as super

      method large     = super#add_class Markup.Card.Primary.large_title_class
      method not_large = super#remove_class Markup.Card.Primary.large_title_class

      method title       = super#text_content
      method set_title s = super#set_text_content s
    end

  end

  module Subtitle = struct

    class t ~subtitle () = object
      inherit [Dom_html.headingElement Js.t] widget (Markup.Card.Primary.create_subtitle ~subtitle ()
                                                     |> To_dom.of_h2) () as super

      method subtitle       = super#text_content
      method set_subtitle s = super#set_text_content s
    end

  end

  class t ~children () =

    let elt = Markup.Card.Primary.create ~children:(List.map (fun x -> Of_dom.of_element x#element) children) ()
              |> To_dom.of_section in

    object
      inherit [Dom_html.element Js.t] widget elt ()
    end

end

module Supporting_text = struct

  class t ~text () =
    let elt = Markup.Card.Supporting_text.create ~children:[Html.pcdata text] ()
              |> To_dom.of_section in
    object
      inherit [Dom_html.element Js.t] widget elt () as super

      method text       = super#text_content
      method set_text s = super#set_text_content s
    end

end

class t ~sections () =
  let elt = Markup.Card.create ~sections:(List.map (fun x -> Of_dom.of_element x#element) sections) ()
            |> To_dom.of_div in

  object

    inherit [Dom_html.divElement Js.t] widget elt ()

  end
