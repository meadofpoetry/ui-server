module CSS = struct
  (** Mandatory. Container for the snackbar elements. *)
  let root = "mdc-snackbar"

  (** Mandatory. Snackbar surface. *)
  let surface = BEM.add_element root "surface"

  (** Mandatory. Message text. *)
  let label = BEM.add_element root "label"

  (** Optonal. Wraps the action button/icon elements, if present. *)
  let actions = BEM.add_element root "actions"

  (** Optional. The action button. *)
  let action = BEM.add_element root "action"

  (** Optional. The dismiss ("X") icon. *)
  let dismiss = BEM.add_element root "dismiss"

  (** Optional. Applied automatically when the snackbar is in the process
      of animating open. *)
  let opening = BEM.add_modifier root "opening"

  (** Optional. Indicates that the snackbar is visible. *)
  let open_ = BEM.add_modifier root "open"

  (** Optional. Applied automatically when the snackbar is in the process
      of anumating closed. *)
  let closing = BEM.add_modifier root "closing"

  (** Optional. Positions the snackbar on the leading edge of the screen
      (left in LTR, right in RTL) instead of centered. *)
  let leading = BEM.add_modifier root "leading"

  (** Optional. Positions the action button/icon below the label instead
      of alongside it. *)
  let stacked = BEM.add_modifier root "stacked"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Button_markup = Button.Make (Xml) (Svg) (Html)
  module Icon_markup = Icon.Make (Xml) (Svg) (Html)
  module Icon_button_markup = Icon_button.Make (Xml) (Svg) (Html)

  let create_action ?(classes = []) =
    let classes = CSS.action :: classes in
    Button_markup.create ~classes

  let create_dismiss ?(classes = []) ?icon =
    let classes = CSS.dismiss :: classes in
    let icon =
      match icon with
      | Some x -> x
      | None -> Icon_markup.SVG.create_of_d Svg_icons.close
    in
    Icon_button_markup.create ~classes ?on_icon:None ?on:None ~icon

  let create_actions ?(classes = []) ?(attrs = []) ?action ?dismiss () : 'a elt =
    let classes = CSS.actions :: classes in
    let actions = Utils.(action ^:: dismiss ^:: []) in
    div ~a:([a_class classes] @ attrs) actions

  let create_label ?(classes = []) ?(attrs = []) ?label ?(content = []) () : 'a elt =
    let classes = CSS.label :: classes in
    div
      ~a:([a_class classes; a_aria "live" ["polite"]; a_role ["status"]] @ attrs)
      (Utils.map_cons_option txt label content)

  let create_surface ?(classes = []) ?(attrs = []) ?action ?dismiss ?actions ?label () :
      'a elt =
    let classes = CSS.surface :: classes in
    let actions =
      match actions, action, dismiss with
      | (Some _ as x), _, _ -> x
      | None, None, None -> None
      | None, Some _, _ | None, _, Some _ -> Some (create_actions ?action ?dismiss ())
    in
    div ~a:([a_class classes] @ attrs) Utils.(label ^:: actions ^:: [])

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(leading = false)
      ?(stacked = false)
      ?dismiss
      ?action
      ?actions
      ?label
      ?(surface = create_surface ?action ?dismiss ?actions ?label ())
      () : 'a elt =
    let (classes : string list) =
      classes
      |> Utils.cons_if leading CSS.leading
      |> Utils.cons_if stacked CSS.stacked
      |> List.cons CSS.root
    in
    div ~a:([a_class classes] @ attrs) [surface]
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
