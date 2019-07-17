open Components_tyxml

module CSS = struct
  let root = "transform"
  let resizer = BEM.add_element root "resizer"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html
  open Utils

  let create_resizer ?(classes = []) ?attrs ?up ?left ?right ?down () : 'a elt =
    let classes = CSS.resizer :: classes in
    div ~a:([ a_class classes
            ; a_role ["slider"]
            ] <@> attrs
            |> map_cons_option (a_user_data "left" % string_of_bool) left
            |> map_cons_option (a_user_data "up" % string_of_bool) up
            |> map_cons_option (a_user_data "right" % string_of_bool) right
            |> map_cons_option (a_user_data "down" % string_of_bool) down)
      [svg ~a:Svg.[ a_x (0., Some `Px)
                  ; a_y (0., Some `Px)
                  ; a_width (64., Some `Px)
                  ; a_height (64., Some `Px)
                  ; a_viewBox (0., 0., 64., 64.)
                  ]
         Svg.[circle ~a:[ a_cx (4., None)
                        ; a_cy (4., None)
                        ; a_r (4., None)
                        ]
                []]
      ]

  let create ?(tabindex = -1) ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([ a_class classes
            ; a_tabindex tabindex
            ; a_role ["slider"]] <@> attrs)
      [ create_resizer ~up:true ~left:true ()
      ; create_resizer ~up:true ~right:true ()
      ; create_resizer ~down:true ~left:true ()
      ; create_resizer ~down:true ~right:true ()
      ]

end
