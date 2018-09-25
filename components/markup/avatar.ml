open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let base_class  = "mdc-avatar"
  let dense_class = CSS.add_modifier base_class "dense"

  module Image = struct
    let create ?(classes=[]) ?attrs ?(dense=false) ~src () =
      img ~a:([ a_class (classes
                         |> cons_if dense dense_class
                         |> List.cons base_class) ] <@> attrs)
        ~src:(uri_of_string src)
        ~alt:""
  end

  module Font_icon = struct
    let icon_class = CSS.add_modifier base_class "icon"

    let create ?(classes=[]) ?attrs ?(dense=false) ~icon () =
      div ~a:([ a_class (classes
                         |> cons_if dense dense_class
                         |> List.cons base_class)] <@> attrs)
        [icon]
  end

  module Letter = struct
    let letter_class = CSS.add_modifier base_class "letter"

    let create ?(classes=[]) ?attrs ?(dense=false) ~text () =
      div ~a:([ a_class (classes
                         |> cons_if dense dense_class
                         |> List.cons letter_class
                         |> List.cons base_class)] <@> attrs)
        [pcdata text]
  end

end
