module CSS = struct
  (** Mandatory, for the list element. *)
  let root = "mdc-list"

  (** Optional, styles the density of the list, making it appear more compact. *)
  let dense = BEM.add_modifier root "dense"

  (** Optional, modifier to style list with two lines (primary and secondary lines). *)
  let two_line = BEM.add_modifier root "two-line"

  (** Optional, configures the leading tiles of each row to display images instead
      of icons. This will make the graphics of the list items larger. *)
  let avatar_list = BEM.add_modifier root "avatar-list"

  (** Optional, disables interactivity affordances. *)
  let non_interactive = BEM.add_modifier root "non-interactive"

  (** Mandatory, for the list item element. *)
  let item = "mdc-list-item"

  (** Mandatory. Wrapper for list item text content (displayed as middle column
      of the list item). *)
  let item_text = BEM.add_element item "text"

  (** Optional, primary text for the list item.
      Should be the child of mdc-list-item__text. *)
  let item_primary_text = BEM.add_element item "primary-text"

  (** Optional, secondary text for the list item. Displayed below the primary text.
      Should be the child of mdc-list-item__text. *)
  let item_secondary_text = BEM.add_element item "secondary-text"

  (** Optional, styles the row in the disabled state. *)
  let item_disabled = BEM.add_modifier item "disabled"

  (** Optional, for list divider element. Can be used between list items OR
      between two lists *)
  let divider = "mdc-list-divider"

  (** Optional, leaves gaps on each side of divider to match padding
      of list-item__meta. *)
  let divider_padded = BEM.add_modifier divider "padded"

  (** Optional, increases the leading margin of the divider so that it
      does not intersect the avatar column. *)
  let divider_inset = BEM.add_modifier divider "inset"

  (** Optional, the first tile in the row (in LTR languages, the first column
      of the list item). Typically an icon or image. *)
  let item_graphic = BEM.add_element item "graphic"

  (** Optional, the last tile in the row (in LTR languages, the last column
      of the list item). Typically small text, icon. or image. *)
  let item_meta = BEM.add_element item "meta"

  (** Optional, wrapper around two or more mdc-list elements to be grouped together. *)
  let group = "mdc-list-group"

  (** Optional, heading text displayed above each list in a group. *)
  let group_subheader = BEM.add_element group "subheader"

  (** Optional, styles the row in the selected state. Selected state should be
      applied on the .mdc-list-item when it is likely to frequently change due
      to user choice. E.g., selecting one or more photos to share in Google
      Photos. *)
  let item_selected = BEM.add_modifier item "selected"

  (** Optional, styles the row in the activated* state. *)
  let item_activated = BEM.add_modifier item "activated"
end

module Role = struct
  module Item = struct
    let option = "option"

    let radio = "radio"

    let checkbox = "checkbox"
  end

  let listbox = "listbox"

  let radiogroup = "radiogroup"

  let group = "group"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  open Utils

  let create_divider
      ?(classes = [])
      ?(attrs = [])
      ?(padded = false)
      ?(inset = false)
      ~tag
      () : 'a elt =
    let (classes : string list) =
      classes
      |> cons_if inset CSS.divider_inset
      |> cons_if padded CSS.divider_padded
      |> List.cons CSS.divider
    in
    tag ~a:([a_class classes; a_role ["separator"]] @ attrs) []

  let create_item_primary_text ?(classes = []) ?(attrs = []) text () : 'a elt =
    let classes = CSS.item_primary_text :: classes in
    span ~a:([a_class classes] @ attrs) [txt text]

  let create_item_secondary_text ?(classes = []) ?(attrs = []) text () : 'a elt =
    let classes = CSS.item_secondary_text :: classes in
    span ~a:([a_class classes] @ attrs) [txt text]

  let create_item_text ?(classes = []) ?(attrs = []) content () : 'a elt =
    let classes = CSS.item_text :: classes in
    span ~a:([a_class classes] @ attrs) content

  let create_item
      ?(classes = [])
      ?(attrs = [])
      ?graphic
      ?meta
      ?role
      ?tabindex
      ?(activated = false)
      ?selected
      ?checked
      text
      () : 'a elt =
    let classes =
      classes
      |> cons_if activated CSS.item_activated
      |> cons_if
           (match selected with
           | None -> false
           | Some x -> x)
           (if activated then CSS.item_activated else CSS.item_selected)
      |> List.cons CSS.item
    in
    li
      ~a:
        ([a_class classes] @ attrs
        |> map_cons_option (fun b -> a_aria "checked" [string_of_bool b]) checked
        |> map_cons_option (fun b -> a_aria "selected" [string_of_bool b]) selected
        |> map_cons_option a_tabindex tabindex
        |> map_cons_option (fun x -> a_role [x]) role)
      (graphic ^:: (text :: (meta ^:: [])))

  let create_item'
      ?(classes = [])
      ?(attrs = [])
      ?graphic
      ?meta
      ?role
      ?tabindex
      ?(activated = false)
      ?selected
      ?checked
      ~text
      (tag : ?a:'b Html.attrib list -> 'c list -> 'a Html.elt) : 'a elt =
    let classes =
      classes
      |> cons_if activated CSS.item_activated
      |> cons_if
           (match selected with
           | None -> false
           | Some x -> x)
           (if activated then CSS.item_activated else CSS.item_selected)
      |> List.cons CSS.item
    in
    tag
      ~a:
        ([a_class classes] @ attrs
        |> map_cons_option (fun b -> a_aria "checked" [string_of_bool b]) checked
        |> map_cons_option (fun b -> a_aria "selected" [string_of_bool b]) selected
        |> map_cons_option a_tabindex tabindex
        |> map_cons_option (fun x -> a_role [x]) role)
      (graphic ^:: (text :: (meta ^:: [])))

  let create_group_subheader ?(classes = []) ?(attrs = []) ?(tag = h3) ~text () : 'a elt
      =
    let classes = CSS.group_subheader :: classes in
    tag ~a:([a_class classes] @ attrs) [txt text]

  let create_group ?(classes = []) ?(attrs = []) ~content () =
    let classes = CSS.group :: classes in
    div ~a:([a_class classes] @ attrs) content

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(avatar_list = false)
      ?(dense = false)
      ?(two_line = false)
      ?(non_interactive = false)
      ?role
      ~items
      () : 'a elt =
    let classes =
      classes
      |> cons_if dense CSS.dense
      |> cons_if two_line CSS.two_line
      |> cons_if avatar_list CSS.avatar_list
      |> cons_if non_interactive CSS.non_interactive
      |> List.cons CSS.root
    in
    ul ~a:([a_class classes] @ attrs |> map_cons_option (fun x -> a_role [x]) role) items
end
