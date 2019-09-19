type action =
  | Close
  | Accept
  | Destroy
  | Custom of string

let action_of_string = function
  | "close" -> Close
  | "accept" -> Accept
  | "destroy" -> Destroy
  | s -> Custom s

let action_to_string = function
  | Close -> "close"
  | Accept -> "accept"
  | Destroy -> "destroy"
  | Custom s -> s

module CSS = struct
  (** Mandatory. The root DOM element containing the surface and the container. *)
  let root = "mdc-dialog"

  (** Mandatory. Wrapper element needed to ensure flexbox behavior in IE 11. *)
  let container = BEM.add_element root "container"

  (** Mandatory. The bounding box for the dialog's content. *)
  let surface = BEM.add_element root "surface"

  (** Mandatory. Semitransparent backdrop that displays behind a dialog. *)
  let scrim = BEM.add_element root "scrim"

  (** Optional. Brief summary of the dialog's purpose. *)
  let title = BEM.add_element root "title"

  (** Optional. Primary content area. May contain a list, a form, or prose. *)
  let content = BEM.add_element root "content"

  (** Optional. Footer area containing the dialog's action buttons. *)
  let actions = BEM.add_element root "actions"

  (** Optional. Individual action button. Typically paired with mdc-button. *)
  let button = BEM.add_element root "button"

  let button_default = BEM.add_modifier button "default"

  (** Optional. Applied automatically when the dialog has overflowing content
      to warrant scrolling. *)
  let scrollable = BEM.add_modifier root "scrollable"

  (** Optional. Indicates that the dialog is open and visible. *)
  let open_ = BEM.add_modifier root "open"

  (** Optional. Applied automatically when the dialog is in the process
      of animating open. *)
  let opening = BEM.add_modifier root "opening"

  (** Optional. Applied automatically when the dialog is in the process
      of animating closed. *)
  let closing = BEM.add_modifier root "closing"

  (** Optional. Applied automatically when the dialog's action buttons can't
      fit on a single line and must be stacked. *)
  let stacked = BEM.add_modifier root "stacked"

  let scroll_lock = "mdc-dialog-scroll-lock"
end

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module CSS = CSS
  module Button = Button.Make (Xml) (Svg) (Html)

  open Utils.Make (Xml)

  let dialog_title ?(classes = []) ?(a = []) ?title ?(children = nil ()) () =
    let classes = return (CSS.title :: classes) in
    let title = Option.map (fun x -> return @@ txt x) title in
    h2 ~a:(a_class classes :: a) (title ^:: children)

  let dialog_content ?(classes = []) ?(a = []) ?(children = nil ()) () =
    let classes = return (CSS.content :: classes) in
    section ~a:(a_class classes :: a) children

  let dialog_action ?(classes = []) ?(a = []) ?(default = false) ?action =
    let classes =
      classes |> Utils.cons_if default CSS.button_default |> List.cons CSS.button
    in
    let a =
      match action with
      | None -> a
      | Some action ->
          let attr = return (action_to_string action) in
          a_user_data "mdc-dialog-action" attr :: a
    in
    Button.button ~classes ~a

  let dialog_actions ?(classes = []) ?(a = []) ?(children = nil ()) () =
    let classes = return (CSS.actions :: classes) in
    footer ~a:(a_class classes :: a) children

  let dialog_surface ?(classes = []) ?(a = []) ?title ?content ?actions () : 'a elt =
    let classes = return (CSS.surface :: classes) in
    let children = title ^:: content ^:: actions ^:: nil () in
    div ~a:(a_class classes :: a) children

  let dialog_container ?(classes = []) ?(a = []) ~surface () : 'a elt =
    let classes = return (CSS.container :: classes) in
    div ~a:(a_class classes :: a) (cons surface (nil ()))

  let dialog_scrim ?(classes = []) ?(a = []) ?(children = nil ()) () : 'a elt =
    let classes = return (CSS.scrim :: classes) in
    div ~a:(a_class classes :: a) children

  let dialog
      ?(classes = [])
      ?(attrs = [])
      ?title_id
      ?content_id
      ?(scrollable = false)
      ?title
      ?content
      ?actions
      ?(scrim = return (dialog_scrim ()))
      ?container
      () : 'a elt =
    let aria n v = a_aria n (return [v]) in
    let container =
      match container with
      | Some x -> x
      | None ->
          let actions =
            match actions with
            | None -> None
            | Some actions -> Some (return (dialog_actions ~children:actions ()))
          in
          let surface = return (dialog_surface ?title ?content ?actions ()) in
          return (dialog_container ~surface ())
    in
    let classes =
      return (classes |> Utils.cons_if scrollable CSS.scrollable |> List.cons CSS.root)
    in
    div
      ~a:
        ([ a_class classes
         ; a_role (return ["alertdialog"])
         ; a_aria "modal" (return ["true"]) ]
         @ attrs
        |> Utils.map_cons_option (aria "labelledby") title_id
        |> Utils.map_cons_option (aria "describedby") content_id)
      (container @:: scrim @:: nil ())
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
