open Components_tyxml

module CSS = struct
  let root = "widget-grid"

  let item = root ^ "-item"

  let item_content = BEM.add_element item "content"

  let item_selected = BEM.add_modifier item "selected"

  let item_dragging = BEM.add_modifier item "dragging"

  let ghost = BEM.add_element root "ghost"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html

  module Text' = Typography.Make(Xml)(Svg)(Html)
  module Icon' = Icon.Make(Xml)(Svg)(Html)
  module Card' = Card.Make(Xml)(Svg)(Html)

  let create_ghost ?(classes = []) ?(attrs = []) () : 'a elt =
    let classes = CSS.ghost :: classes in
    div ~a:([a_class classes] @ attrs) []

  let create_icon ?classes ?attrs
      (widget : Pipeline_types.Wm.widget) : 'a elt =
    let path = match widget.type_ with
      | Video -> Svg_icons.video
      | Audio -> Svg_icons.music in
    Icon'.SVG.(create ?classes ?attrs [create_path path ()] ())

  let create_content ?(classes = []) ?(attrs = [])
      (widget : Pipeline_types.Wm.widget) : 'a elt =
    let pid = match widget.pid with
      | None -> None
      | Some pid ->
        let text = Printf.sprintf "PID: %d" pid in
        Some (Text'.make text) in
    let ( ^:: ) x l = match x with None -> l | Some x -> x :: l in
    let text = Text'.make widget.description in
    let icon = create_icon widget in
    let classes = CSS.item_content :: classes in
    div ~a:([a_class classes] @ attrs)
      (icon :: (pid ^:: [text]))

  let create_item ?(classes = []) ?(attrs = []) ?content
      (widget : Pipeline_types.Wm.widget) : 'a elt =
    let classes = CSS.item :: classes in
    let content = match content with
      | None -> [create_content widget]
      | Some x -> x in
    div ~a:([a_class classes] @ attrs) content

  let create ?(classes = []) ?(attrs = []) ?(content = []) () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([ a_class classes
            ; a_role ["grid"]
            ] @ attrs)
      content
end
