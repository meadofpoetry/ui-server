module CSS = struct
  let root = "mdc-icon"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  module SVG = struct
    let create_path ?(classes = []) ?(attrs = []) ?fill ~d () : 'a Svg.elt =
      Svg.path
        ~a:
          ([Svg.a_class classes; Svg.a_d d] @ attrs
          |> Utils.map_cons_option Svg.a_fill fill)
        []

    let create ?(classes = []) ?(attrs = []) ?(size = 24) ?fill ?d ?(children = []) () =
      let sz = float_of_int size in
      svg
        ~a:
          Svg.(
            [ a_class (CSS.root :: classes)
            ; a_width (sz, None)
            ; a_height (sz, None)
            ; a_viewBox (0., 0., sz, sz) ]
            @ attrs)
        (Utils.map_cons_option (fun d -> create_path ?fill ~d ()) d children)
  end
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
