open Utils

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

  let create_label ?(classes = []) ?(attrs = []) (text : string) () : 'a elt =
    let classes = CSS.label :: classes in
    div
      ~a:([a_class classes; a_aria "live" ["polite"]; a_role ["status"]] @ attrs)
      [txt text]

  let create_action ?(classes = []) ?(attrs = []) (label : string) () : 'a elt =
    let classes = CSS.action :: Button.CSS.root :: classes in
    button ~a:([a_class classes] @ attrs) [txt label]

  let create_actions ?(classes = []) ?(attrs = []) ?action ?dismiss () : 'a elt =
    let classes = CSS.actions :: classes in
    let actions = action ^:: dismiss ^:: [] in
    div ~a:([a_class classes] @ attrs) actions

  let create_surface ?(classes = []) ?(attrs = []) ?actions ~label () : 'a elt =
    let classes = CSS.surface :: classes in
    div
      ~a:([a_class classes] @ attrs)
      (match actions with
      | None -> [label]
      | Some actions -> [label; actions])

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(leading = false)
      ?(stacked = false)
      ~(surface : 'a elt)
      () : 'a elt =
    let (classes : string list) =
      classes
      |> cons_if leading CSS.leading
      |> cons_if stacked CSS.stacked
      |> List.cons CSS.root
    in
    div ~a:([a_class classes] @ attrs) [surface]
end
