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
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Button_markup = Button.Make (Xml) (Svg) (Html)

  let create_title ?(classes = []) ?(attrs = []) ?title ?(content = []) () : 'a elt =
    let classes = CSS.title :: classes in
    h2 ~a:([a_class classes] @ attrs) (Utils.map_cons_option txt title content)

  let create_content ?(classes = []) ?(attrs = []) content : 'a elt =
    let classes = CSS.content :: classes in
    section ~a:([a_class classes] @ attrs) content

  let action ?(classes = []) ?(a = []) ?(default = false) ?action =
    let classes =
      classes |> Utils.cons_if default CSS.button_default |> List.cons CSS.button
    in
    let a =
      match action with
      | None -> a
      | Some action ->
          let attr = action_to_string action in
          a_user_data "mdc-dialog-action" attr :: a
    in
    Button_markup.button ~classes ~a

  let create_actions ?(classes = []) ?(attrs = []) actions : 'a elt =
    let classes = CSS.actions :: classes in
    footer ~a:([a_class classes] @ attrs) actions

  let create_surface ?(classes = []) ?(attrs = []) ?title ?content ?actions () : 'a elt =
    let classes = CSS.surface :: classes in
    let content = Utils.(title ^:: content ^:: actions ^:: []) in
    div ~a:([a_class classes] @ attrs) content

  let create_container ?(classes = []) ?(attrs = []) ~surface () : 'a elt =
    let classes = CSS.container :: classes in
    div ~a:([a_class classes] @ attrs) [surface]

  let create_scrim ?(classes = []) ?(attrs = []) () : 'a elt =
    let classes = CSS.scrim :: classes in
    div ~a:([a_class classes] @ attrs) []

  let create
      ?(classes = [])
      ?(attrs = [])
      ?title_id
      ?content_id
      ?(scrollable = false)
      ?title
      ?content
      ?actions
      ?(scrim = create_scrim ())
      ?container
      () : 'a elt =
    let aria n v = a_aria n [v] in
    let container =
      match container with
      | Some x -> x
      | None ->
          let actions =
            match actions with
            | None -> None
            | Some actions -> Some (create_actions actions)
          in
          let surface = create_surface ?title ?content ?actions () in
          create_container ~surface ()
    in
    let classes =
      classes |> Utils.cons_if scrollable CSS.scrollable |> List.cons CSS.root
    in
    div
      ~a:
        ([a_class classes; a_role ["alertdialog"]; a_aria "modal" ["true"]] @ attrs
        |> Utils.map_cons_option (aria "labelledby") title_id
        |> Utils.map_cons_option (aria "describedby") content_id)
      [container; scrim]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
