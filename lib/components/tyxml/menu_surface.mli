module CSS : sig
  val root : string
  (** Mandatory. *)

  val animating_open : string
  (** Indicates the menu surface is currently animating open.
      This class is removed once the animation completes. *)

  val open_ : string
  (** Indicates the menu surface is currently open, or is currently animating open. *)

  val animating_closed : string
  (** Indicates the menu surface is currently animating closed.
      This class is removed once the animation completes. *)

  val anchor : string
  (** Used to indicate which element the menu should be anchored to. *)

  val fixed : string
  (** Used to indicate that the menu is using fixed positioning. *)
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml with module Svg := Svg) : sig
  val create :
       ?classes:string list
    -> ?attrs:Html_types.div_attrib Html.attrib list
    -> ?fixed:bool
    -> ?open_:bool
    -> [< Html_types.div_content_fun] Html.elt list
    -> unit
    -> [> Html_types.div] Html.elt
end
