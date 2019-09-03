module CSS = struct
  open Components_tyxml

  let root = "mdc-avatar"

  let dense = BEM.add_modifier root "dense"

  let icon = BEM.add_modifier root "icon"

  let letter = BEM.add_modifier root "letter"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap with module Xml := Xml
                                   and module Svg := Svg) = struct
  open Html

  module Image = struct
    let create ?(classes = []) ?(attrs = []) ?(dense = false) ~src () =
      let classes =
        classes
        |> Components_tyxml.Utils.cons_if dense CSS.dense
        |> List.cons CSS.root in
      img ~a:([a_class classes] @ attrs) ~src:(uri_of_string src) ~alt:""
  end

  module Font_icon = struct
    let create ?(classes = []) ?(attrs = []) ?(dense = false) ~icon () =
      let classes =
        classes
        |> Components_tyxml.Utils.cons_if dense CSS.dense
        |> List.cons CSS.root in
      div ~a:([a_class classes] @ attrs) [icon]
  end

  module Letter = struct
    let create ?(classes = []) ?(attrs = []) ?(dense = false) ~text () =
      let classes =
        classes
        |> Components_tyxml.Utils.cons_if dense CSS.dense
        |> List.cons CSS.letter
        |> List.cons CSS.root in
      div ~a:([a_class classes] @ attrs) [txt text]
  end
end
