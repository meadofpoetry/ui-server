module CSS : sig
  val root : string
  (** Mandatory, for the list element. *)

  val dense : string
  (** Optional, styles the density of the list, making it appear more compact. *)

  val two_line : string
  (** Optional, modifier to style list with two lines (primary and secondary lines). *)

  val avatar_list : string
  (** Optional, configures the leading tiles of each row to display images instead
      of icons. This will make the graphics of the list items larger. *)

  val non_interactive : string
  (** Optional, disables interactivity affordances. *)

  val item : string
  (** Mandatory, for the list item element. *)

  val item_text : string
  (** Mandatory. Wrapper for list item text content (displayed as middle column
      of the list item). *)

  val item_primary_text : string
  (** Optional, primary text for the list item.
      Should be the child of mdc-list-item__text. *)

  val item_secondary_text : string
  (** Optional, secondary text for the list item. Displayed below the primary text.
      Should be the child of mdc-list-item__text. *)

  val item_disabled : string
  (** Optional, styles the row in the disabled state. *)

  val divider : string
  (** Optional, for list divider element. Can be used between list items OR
      between two lists *)

  val divider_padded : string
  (** Optional, leaves gaps on each side of divider to match padding
      of list-item__meta. *)

  val divider_inset : string
  (** Optional, increases the leading margin of the divider so that it
      does not intersect the avatar column. *)

  val item_graphic : string
  (** Optional, the first tile in the row (in LTR languages, the first column
      of the list item). Typically an icon or image. *)

  val item_meta : string
  (** Optional, the last tile in the row (in LTR languages, the last column
      of the list item). Typically small text, icon. or image. *)

  val group : string
  (** Optional, wrapper around two or more mdc-list elements to be grouped together. *)

  val group_subheader : string
  (** Optional, heading text displayed above each list in a group. *)

  val item_selected : string
  (** Optional, styles the row in the selected state. Selected state should be
      applied on the .mdc-list-item when it is likely to frequently change due
      to user choice. E.g., selecting one or more photos to share in Google
      Photos. *)

  val item_activated : string
  (** Optional, styles the row in the activated* state. *)
end

module Role : sig
  module Item : sig
    val option : string

    val radio : string

    val checkbox : string
  end

  val listbox : string

  val radiogroup : string

  val group : string
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml with module Svg := Svg) : sig
  open Html

  val create_divider :
       ?classes:string list
    -> ?attrs:([> `Class | `Role] as 'b) attrib list
    -> ?padded:bool
    -> ?inset:bool
    -> tag:(a:'b attrib list -> 'c list -> 'a elt)
    -> unit
    -> 'a elt

  val create_item_primary_text :
       ?classes:string list
    -> ?attrs:Html_types.span_attrib attrib list
    -> string
    -> unit
    -> Html_types.span elt

  val create_item_secondary_text :
       ?classes:string list
    -> ?attrs:Html_types.span_attrib attrib list
    -> string
    -> unit
    -> Html_types.span elt

  val create_item_text :
       ?classes:string list
    -> ?attrs:Html_types.span_attrib attrib list
    -> [< Html_types.span_content_fun] elt list
    -> unit
    -> Html_types.span elt

  val create_item :
       ?classes:string list
    -> ?attrs:Html_types.li_attrib attrib list
    -> ?graphic:([< Html_types.li_content_fun] as 'a) elt
    -> ?meta:'a elt
    -> ?role:string
    -> ?tabindex:int
    -> ?activated:bool
    -> ?selected:bool
    -> ?checked:bool
    -> 'a elt
    -> unit
    -> Html_types.li elt

  val create_item' :
       ?classes:string list
    -> ?attrs:([> `Aria | `Class | `Role | `Tabindex] as 'b) Html.attrib list
    -> ?graphic:'c
    -> ?meta:'c
    -> ?role:string
    -> ?tabindex:Html_types.number Html.wrap
    -> ?activated:bool
    -> ?selected:bool
    -> ?checked:bool
    -> text:'c
    -> (?a:'b Html.attrib list -> 'c list -> 'a Html.elt)
    -> 'a Html.elt

  val create_group_subheader :
       ?classes:string list
    -> ?attrs:(Html_types.h3_attrib as 'a) attrib list
    -> ?tag:('a, Html_types.h3_content_fun, Html_types.h3) star
    -> text:string
    -> unit
    -> Html_types.h3 elt

  val create_group :
       ?classes:string list
    -> ?attrs:Html_types.div_attrib attrib list
    -> content:[< Html_types.div_content_fun] elt list
    -> unit
    -> Html_types.div elt

  val create :
       ?classes:string list
    -> ?attrs:Html_types.ul_attrib attrib list
    -> ?avatar_list:bool
    -> ?dense:bool
    -> ?two_line:bool
    -> ?non_interactive:bool
    -> ?role:string
    -> items:[< Html_types.ul_content_fun] elt list
    -> unit
    -> Html_types.ul elt
end
