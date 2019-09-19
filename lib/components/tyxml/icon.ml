module CSS = struct
  let root = "mdc-icon"
end

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module CSS = CSS

  module SVG = struct
    let icon_path ?(classes = []) ?(a = []) ?fill ~d ?(children = Xml.W.nil ()) () =
      Svg.path
        ~a:
          (Svg.a_class (return classes) :: Svg.a_d d :: a
          |> Utils.map_cons_option Svg.a_fill fill)
        children

    let icon
        ?(classes = [])
        ?(a = [])
        ?(size = 24)
        ?fill
        ?d
        ?(children = Xml.W.nil ())
        () =
      let sz = float_of_int size in
      let classes = CSS.root :: classes in
      let path = Option.map (fun d -> return @@ icon_path ?fill ~d ()) d in
      svg
        ~a:
          (Svg.
             [ a_class (return classes)
             ; a_width (return (sz, None))
             ; a_height (return (sz, None))
             ; a_viewBox (return (0., 0., sz, sz)) ]
          @ a)
        (match path with
        | None -> children
        | Some x -> cons x children)
  end
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
