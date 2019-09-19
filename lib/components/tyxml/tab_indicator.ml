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
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module CSS = CSS

  let tab_indicator_content ?(classes = []) ?(a = []) ?icon () : 'a elt =
    let children, content_class =
      match icon with
      | None -> nil (), CSS.content_underline
      | Some i -> singleton (return i), CSS.content_icon
    in
    let classes = CSS.content :: content_class :: classes in
    span ~a:(a_class (return classes) :: a) children

  let tab_indicator
      ?(classes = [])
      ?(a = [])
      ?(active = false)
      ?(fade = false)
      ?icon
      ?(content = tab_indicator_content ?icon ())
      () =
    let (classes : string list) =
      classes
      |> Utils.cons_if fade CSS.fade
      |> Utils.cons_if active CSS.active
      |> List.cons CSS.root
    in
    span ~a:(a_class (return classes) :: a) (singleton (return content))
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
