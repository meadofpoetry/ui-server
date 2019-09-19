module CSS = struct
  let root = "mdc-icon"
end

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module CSS = CSS

  let ( ^:: ) x l =
    match x with
    | None -> l
    | Some x -> Xml.W.cons x l

  module SVG = struct
    let icon_path
        ?(classes = Xml.W.return [])
        ?(a = [])
        ?fill
        ~d
        ?(children = Xml.W.nil ())
        () =
      Svg.path
        ~a:
          (Svg.a_class classes :: Svg.a_d d :: a |> Utils.map_cons_option Svg.a_fill fill)
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
      let classes = Xml.W.return (CSS.root :: classes) in
      let path = Option.map (fun d -> Xml.W.return @@ icon_path ?fill ~d ()) d in
      svg
        ~a:
          (Svg.
             [ a_class classes
             ; a_width (Xml.W.return (sz, None))
             ; a_height (Xml.W.return (sz, None))
             ; a_viewBox (Xml.W.return (0., 0., sz, sz)) ]
          @ a)
        (path ^:: children)
  end
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
