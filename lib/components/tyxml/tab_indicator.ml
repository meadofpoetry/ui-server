module CSS = struct
  (** Mandatory. Contains the tab indicator content. *)
  let root = "mdc-tab-indicator"

  (** Mandatory. Denotes the tab indicator content. *)
  let content = BEM.add_element root "content"

  (** Optional. Visually activates the indicator. *)
  let active = BEM.add_modifier root "active"

  (** Optional. Sets up the tab indicator to fade in on activation
      and fade out on deactivation. *)
  let fade = BEM.add_modifier root "fade"

  (** Optional. Denotes an underline tab indicator. *)
  let content_underline = BEM.add_modifier content "underline"

  (** Optional. Denotes an icon tab indicator. *)
  let content_icon = BEM.add_modifier content "icon"

  let no_transition = BEM.add_modifier root "no-transition"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_content ?(classes = []) ?(attrs = []) ?icon () : 'a elt =
    let children, content_class =
      match icon with
      | None -> [], CSS.content_underline
      | Some i -> [i], CSS.content_icon
    in
    let classes = CSS.content :: content_class :: classes in
    span ~a:([a_class classes] @ attrs) children

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(active = false)
      ?(fade = false)
      ?icon
      ?(content = create_content ?icon ())
      () =
    let (classes : string list) =
      classes
      |> Utils.cons_if fade CSS.fade
      |> Utils.cons_if active CSS.active
      |> List.cons CSS.root
    in
    span ~a:([a_class classes] @ attrs) [content]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
