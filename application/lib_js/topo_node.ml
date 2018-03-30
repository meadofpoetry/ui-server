open Containers
open Components

let port_section_height = 50
let base_class          = "topology__node"

module Header = struct

  class t ?action ?subtitle ~title () =
    let _class     = Markup.CSS.add_element base_class "header" in
    let title_w    = new Card.Primary.title title () in
    let subtitle_w = Option.map (fun x -> (new Card.Primary.subtitle x ())#widget) subtitle in
    let box        = new Box.t
                         ~vertical:true
                         ~widgets:([]
                                   |> List.cons_maybe subtitle_w
                                   |> List.cons title_w#widget)
                         () in
    object(self)
      inherit Card.Primary.t ~widgets:([]
                                       |> List.cons_maybe action
                                       |> List.cons box#widget)
                             ()
      initializer
        self#add_class _class
    end

end

module Body = struct

  class t n () =
  object(self)
    inherit Card.Media.t ~widgets:[] ()
    initializer
      self#style##.height := Js.string @@ Utils.px (n * port_section_height)
  end

end

class t ~(header:#Header.t) ~(body:#Body.t) () =
object(self)
  inherit Card.t ~widgets:[header#widget;body#widget] ()
  initializer
    self#add_class base_class
end
