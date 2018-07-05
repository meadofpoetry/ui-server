open Containers
open Tyxml_js

module Markup = Components_markup.Icon.Make(Xml)(Svg)(Html)

module Font = struct

  class t ~icon () =
    let elt = Markup.Font.create ~icon () |> Tyxml_js.To_dom.of_i in
    object
      inherit Widget.button_widget elt () as super
      method icon       = super#text_content |> Option.get_or ~default:""
      method set_icon i = super#set_text_content i
    end

end

module Button = struct

  module Font = struct

    class t ~icon () = object(self)
      val mutable _ripple = None
      inherit Font.t ~icon ()

      method set_disabled x = self#add_or_remove_class x Markup.disabled_class
      method disabled       = self#has_class Markup.disabled_class
      method layout ()      = Option.iter (fun r -> r##layout ()) _ripple

      initializer
        self#add_class Markup.button_class;
        (let r = Ripple.attach self in
         _ripple <- Some r;
         self#add_class "mdc-ripple-surface"; (*FIXME*)
         r##.unbounded := Js.bool true;
         Ripple.set_unbounded self)
    end

  end

end
