module CSS = struct
  let root = "mdc-icon"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  module SVG = struct
    let icon_path ?(classes = return []) ?(a = []) ?fill ~d ?(children = nil ()) () =
      Svg.path
        ~a:
          (Svg.a_class classes :: Svg.a_d d :: a |> Utils.map_cons_option Svg.a_fill fill)
        children

    let icon
        ?(classes = return [])
        ?(a = [])
        ?(size = return 24)
        ?fill
        ?d
        ?(children = nil ())
        () =
      let sz = fmap (fun x -> float_of_int x, None) size in
      let viewbox = fmap (fun x -> 0., 0., float_of_int x, float_of_int x) size in
      let classes = fmap (List.cons CSS.root) classes in
      let path = Option.map (fun d -> return @@ icon_path ?fill ~d ()) d in
      svg
        ~a:
          (Svg.a_class classes
          :: Svg.a_width sz
          :: Svg.a_height sz
          :: Svg.a_viewBox viewbox
          :: a)
        (match path with
        | None -> children
        | Some x -> cons x children)
  end
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
