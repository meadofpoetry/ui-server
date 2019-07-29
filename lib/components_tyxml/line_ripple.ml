module CSS = struct
  (** Mandatory. *)
  let root = "mdc-line-ripple"

  (** Styles the line ripple as an active line ripple. *)
  let active = BEM.add_modifier root "active"

  (** Styles the line ripple as a deactivating line ripple. *)
  let deactivating = BEM.add_modifier root "deactivating"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  let create ?(classes = []) ?(attrs = []) () : 'a Html.elt =
    let classes = CSS.root :: classes in
    Html.(div ~a:([a_class classes] @ attrs) [])
end
