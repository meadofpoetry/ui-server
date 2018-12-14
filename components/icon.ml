open Containers
open Tyxml_js

module Markup = Components_markup.Icon.Make(Xml)(Svg)(Html)

module Font = struct

  module Markup = Markup.Font

  class t ~icon () =
    let elt = Markup.create ~icon () |> Tyxml_js.To_dom.of_i in
    object
      inherit Widget.button_widget elt () as super

      method icon : string =
        super#text_content |> Option.get_or ~default:""
      method set_icon (i : string) : unit =
        super#set_text_content i
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

    class t ?(fill : Color.t option) path () =
      let fill = Option.map Color.to_css_rgba fill in
      let elt  = Markup.create_path ?fill path ()
                 |> To_dom.of_element in
      object(self)
        inherit Widget.t elt ()

        method get : string =
          Option.get_or ~default:"" @@ self#get_attribute "d"
        method set (s : string) : unit =
          self#set_attribute "d" s

    end

  end

  class t ~(paths : Path.t list) () =
    let paths' = List.map (fun x -> Of_dom.of_element x#root) paths in
    let elt = Markup.create paths' ()
              |> Tyxml_js.To_dom.of_element in
    object(self)
      inherit Widget.button_widget elt ()

      method paths : Path.t list =
        paths
      method path : Path.t =
        List.hd self#paths
    end

  let create_simple (path : string) : t =
    let path = new Path.t path () in
    new t ~paths:[ path ] ()

end
