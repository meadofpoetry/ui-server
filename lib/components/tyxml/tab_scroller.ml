type align =
  | Start
  | Center
  | End

module CSS = struct
  (** Mandatory. Contains the tab scroller content. *)
  let root = "mdc-tab-scroller"

  let scroll_test = BEM.add_element root "test"

  (** Mandatory. Denotes the scrolling area. *)
  let scroll_area = BEM.add_element root "scroll-area"

  let scroll_area_scroll = BEM.add_modifier scroll_area "scroll"

  (** Mandatory. Denotes the scrolling content. *)
  let scroll_content = BEM.add_element root "scroll-content"

  (** Optional. Sets the elements inside the scroll content element
      to be aligned to the start of the scroll content element. *)
  let align_start = BEM.add_modifier root "align-start"

  (** Optional. Sets the elements inside the scroll content element to be
      aligned to the end of the scroll content element. *)
  let align_end = BEM.add_modifier root "align-end"

  (** Optional. Sets the elements inside the scroll content element to be
      aligned to the center of the scroll content element. *)
  let align_center = BEM.add_modifier root "align-center"

  let animating = BEM.add_modifier root "animating"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_scroll_content ?(classes = []) ?(attrs = []) ?(tabs = []) () : 'a elt =
    let classes = CSS.scroll_content :: classes in
    div ~a:([a_class classes] @ attrs) tabs

  let create_scroll_area
      ?(classes = [])
      ?(attrs = [])
      ?tabs
      ?(scroll_content = create_scroll_content ?tabs ())
      () : 'a elt =
    let classes = CSS.scroll_area :: classes in
    div ~a:([a_class classes] @ attrs) [scroll_content]

  let create
      ?(classes = [])
      ?(attrs = [])
      ?align
      ?tabs
      ?(scroll_area = create_scroll_area ?tabs ())
      () : 'a elt =
    let align =
      match align with
      | None -> None
      | Some Start -> Some CSS.align_start
      | Some Center -> Some CSS.align_center
      | Some End -> Some CSS.align_end
    in
    let classes = CSS.root :: Utils.(align ^:: classes) in
    div ~a:([a_class classes] @ attrs) [scroll_area]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
