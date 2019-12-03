module type Common_css = sig
  val root : string

  val dismissible : string

  val modal : string

  val open_ : string

  val opening : string

  val closing : string

  val animate : string

  val content : string

  val scrim : string
end

module Make_css (M : sig
  val root : string
end) : sig
  include Common_css

  val scrim : string
end = struct
  (** Mandatory. *)
  let root = M.root

  (** Dismissible side-sheet variant class. *)
  let dismissible = BEM.add_modifier root "dismissible"

  (** Modal side-sheet variant class. *)
  let modal = BEM.add_modifier root "modal"

  (** If present, indicates that the dismissible side-sheet is in the open position .*)
  let open_ = BEM.add_modifier root "open"

  (** Applied while the side-sheet is animating from the closed to the open position. *)
  let opening = BEM.add_modifier root "opening"

  (** Applied while the side-sheet is animating from the open to the closed position. *)
  let closing = BEM.add_modifier root "closing"

  let animate = BEM.add_modifier root "animate"

  (** Scrollable content area of the side-sheet. *)
  let content = BEM.add_element root "content"

  (** Mandatory for modal variant only. Used for the overlay on the app content. *)
  let scrim = root ^ "-scrim"
end

type typ =
  | Modal
  | Dismissible
  | Permanent

let typ_to_string = function
  | Modal -> "modal"
  | Dismissible -> "dismissible"
  | Permanent -> "permanent"

module CSS = struct
  include Make_css (struct
    let root = "mdc-side-sheet"
  end)

  (** Mandatory for dismissible variant only. Sibling element that is resized
      when the side-sheet opens/closes. *)
  let app_content = root ^ "-app-content"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let side_sheet_scrim ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (List.cons CSS.scrim) classes in
    div ~a:(a_class classes :: a) children

  let side_sheet_content ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (List.cons CSS.content) classes in
    div ~a:(a_class classes :: a) children

  let side_sheet ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (List.cons CSS.root) classes in
    aside ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
