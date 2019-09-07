module CSS = struct
  (** Mandatory. The main card element. *)
  let root = "mdc-card"

  (** Optional. The main tappable area of the card. Typically contains most
      (or all) card content except mdc-card__actions. Only applicable to cards
      that have a primary action that the main surface should trigger. *)
  let primary_action = BEM.add_element root "primary-action"

  (** Optional. Media area that displays a custom background-image with b
      ackground-size: cover. *)
  let media = BEM.add_element root "media"

  (** Optional. An absolutely-positioned box the same size as the media area,
      for displaying a title or icon on top of the background-image. *)
  let media_content = BEM.add_element root "media-content"

  (** Optional. Row containing action buttons and/or icons. *)
  let actions = BEM.add_element root "actions"

  (** Optional. An individual action button or icon. *)
  let action = BEM.add_element root "action"

  (** Optional. A group of supplemental action icons, displayed on the right
      side of the card (in LTR), adjacent to __action-buttons. *)
  let action_icons = BEM.add_element root "action-icons"

  (** Optional. A group of action buttons, displayed on the left side of the
      card (in LTR), adjacent to mdc-card__action-icons. *)
  let action_buttons = BEM.add_element root "action-buttons"

  (** Optional. Removes the shadow and displays a hairline outline instead. *)
  let outlined = BEM.add_modifier root "outlined"

  (** Optional. Automatically scales the media area's height to equal its width. *)
  let media_square = BEM.add_modifier media "square"

  (** Optional. Automatically scales the media area's height according to its
      width, maintaining a 16:9 aspect ratio. *)
  let media_16_9 = BEM.add_modifier media "16-9"

  (** Optional. Removes the action area's padding and causes its only child
      (an mdc-card__action element) to consume 100% of the action area's width. *)
  let actions_full_bleed = BEM.add_modifier actions "full-bleed"

  (** Optional. An action button with text. *)
  let action_button = BEM.add_modifier action "button"

  (** Optional. An action icon with no text. *)
  let action_icon = BEM.add_modifier action "icon"

  let primary = BEM.add_element root "primary"

  let title = BEM.add_element root "title"

  let subtitle = BEM.add_element root "subtitle"

  let overline = BEM.add_element root "overline"

  let title_large = BEM.add_modifier title "large"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_media ?(classes = []) ?(attrs = []) ?(tag = div) children : 'a elt =
    let classes = CSS.media :: classes in
    tag ~a:([a_class classes] @ attrs) children

  let create_action_buttons ?(classes = []) ?(attrs = []) buttons : 'a elt =
    let classes = CSS.action_buttons :: classes in
    div ~a:([a_class classes] @ attrs) buttons

  let create_action_icons ?(classes = []) ?(attrs = []) icons : 'a elt =
    let classes = CSS.action_icons :: classes in
    div ~a:([a_class classes] @ attrs) icons

  let create_actions ?(classes = []) ?(attrs = []) content : 'a elt =
    let classes = CSS.actions :: classes in
    section ~a:([a_class classes] @ attrs) content

  let create_overline ?(classes = []) ?(attrs = []) text : 'a elt =
    let classes = CSS.overline :: classes in
    h5 ~a:([a_class classes] @ attrs) [txt text]

  let create_title ?(classes = []) ?(attrs = []) ?(large = false) text : 'a elt =
    let classes =
      classes |> Utils.cons_if large CSS.title_large |> List.cons CSS.title
    in
    h2 ~a:([a_class classes] @ attrs) [txt text]

  let create_subtitle ?(classes = []) ?(attrs = []) text : 'a elt =
    let classes = CSS.subtitle :: classes in
    h3 ~a:([a_class classes] @ attrs) [txt text]

  let create_primary ?(classes = []) ?(attrs = []) content : 'a elt =
    let classes = CSS.primary :: classes in
    section ~a:([a_class classes] @ attrs) content

  let create ?(classes = []) ?(attrs = []) ?(tag = div) ?(outlined = false) sections :
      'a elt =
    let classes = classes |> Utils.cons_if outlined CSS.outlined |> List.cons CSS.root in
    tag ~a:([a_class classes] @ attrs) sections
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
