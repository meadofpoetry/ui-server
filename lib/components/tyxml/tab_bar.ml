module CSS = struct
  (** Mandatory. *)
  let root = "mdc-tab-bar"
end

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let create ?(classes = []) ?(attrs = []) ~scroller () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([ a_class classes
            ; a_role  ["tablist"]]
            @ attrs) [scroller]

end
