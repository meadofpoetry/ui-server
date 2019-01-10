open Utils

module type Common_css = sig
  val root : string
  val dismissible : string
  val modal : string
  val open_ : string
  val opening : string
  val closing : string
  val animate : string
  val content : string
end

module Make_css(M : sig val root : string end) : sig
  include Common_css
  include module type of CSS
  val scrim : string
end = struct
  include CSS
  let root = M.root
  let dismissible = add_modifier root "dismissible"
  let modal = add_modifier root "modal"
  let open_ = add_modifier root "open"
  let opening = add_modifier root "opening"
  let closing = add_modifier root "closing"
  let animate = add_modifier root "animate"
  let content = add_element root "content"
  let scrim = root ^ "-scrim"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module CSS = struct
    include Make_css(struct let root = "mdc-side-sheet" end)
    let app_content = root ^ "-app-content"
  end

  let create_scrim ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.scrim :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create_content ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.content :: classes in
    div ~a:([a_class classes] <@> attrs) content

  let create ?(classes = []) ?attrs content () : 'a elt =
    let classes = CSS.root :: classes in
    aside ~a:([a_class classes] <@> attrs) [content]

end
