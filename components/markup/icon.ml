open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  type size = [ `S18 | `S24 | `S32 | `S48 ]

  let base_class = "mdc-icon"

  module Font = struct
    let create ?(classes=[]) ?attrs ~icon () =
      Html.i ~a:([ a_class ("material-icons" :: base_class :: classes) ]
                 <@> attrs)
        [ pcdata icon ]
  end

  module SVG = struct

    module Path = Svg_icons

    let create_path ?(classes=[]) ?attrs ?fill d () =
      Svg.path ~a:([ Svg.a_class classes
                   ; Svg.a_d d ]
                   |> map_cons_option (fun x -> Svg.a_fill @@ `Color (x, None))
                        fill
                   <@> attrs) []

    let create ?(classes=[]) ?attrs ?(size=24) paths () =
      let sz_fl = float_of_int size in
      svg ~a:([ Svg.a_class (base_class :: classes)
              ; Svg.a_width (sz_fl, None)
              ; Svg.a_height (sz_fl, None)
              ; Svg.a_viewBox (0., 0., sz_fl, sz_fl)
              ] <@> attrs)
        paths

  end

end
