module Settings_section = Settings_section

module CSS = struct
  let root = "network-settings"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html

  let make ?(classes = []) ?(attrs = []) sections =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] @ attrs) sections

end
