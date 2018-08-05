open Containers
open Tyxml_js

module Markup = Components_markup.Icon.Make(Xml)(Svg)(Html)

module Font = struct

  module Markup = Markup.Font

  class t ~icon () =
    let elt = Markup.create ~icon () |> Tyxml_js.To_dom.of_i in
    object
      inherit Widget.button_widget elt () as super
      method icon       = super#text_content |> Option.get_or ~default:""
      method set_icon i = super#set_text_content i
    end

end

module SVG = struct

  module Markup = Markup.SVG

  module To_dom = Tyxml_cast.MakeTo(struct
                      type 'a elt = 'a Tyxml_js.Svg.elt
                      let elt = Tyxml_js.Svg.toelt
                    end)

  module Of_dom = Tyxml_cast.MakeOf(struct
                      type 'a elt = 'a Tyxml_js.Svg.elt
                      let elt = Tyxml_js.Svg.tot
                    end)

  module Path = struct

    include Markup.Path

    class t ?(fill:Color.t option) path () =
      let fill = Option.map Color.string_of_t fill in
      let elt  = Markup.create_path ?fill path ()
                 |> To_dom.of_element in
      object(self)
        inherit Widget.t elt ()

        method get : string =
          Option.get_or ~default:"" @@ self#get_attribute "d"
        method set (s:string) : unit =
          self#set_attribute "d" s

    end

  end

  class t ~(paths:Path.t list) () =
    let paths' = List.map (fun x -> Of_dom.of_element x#root) paths in
    let elt = Markup.create paths' ()
              |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.button_widget elt ()

      method paths = paths
      method path = List.hd self#paths
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
