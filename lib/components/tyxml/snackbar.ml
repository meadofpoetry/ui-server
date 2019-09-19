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
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module CSS = CSS
  module Button = Button.Make (Xml) (Svg) (Html)
  module Icon = Icon.Make (Xml) (Svg) (Html)
  module Icon_button = Icon_button.Make (Xml) (Svg) (Html)

  let ( ^:: ) x l =
    match x with
    | None -> l
    | Some x -> cons x l

  let snackbar_action ?(classes = []) =
    let classes = CSS.action :: classes in
    Button.button ~classes

  let snackbar_dismiss ?(classes = []) ?icon =
    let classes = CSS.dismiss :: classes in
    let icon =
      match icon with
      | Some x -> x
      | None -> return (Icon.SVG.icon ~d:(return Svg_icons.close) ())
    in
    Icon_button.icon_button ~classes ?on_icon:None ?on:None ~icon

  let snackbar_actions ?(classes = []) ?(a = []) ?action ?dismiss ?children () : 'a elt =
    let classes = CSS.actions :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let action =
            match action with
            | None -> None
            | Some (`Text s) -> Some (return @@ snackbar_action ~label:s ())
            | Some (`Element e) -> Some e
          in
          let dismiss =
            match dismiss with
            | None -> None
            | Some `True -> Some (return @@ snackbar_dismiss ())
            | Some (`Element e) -> Some e
          in
          action ^:: dismiss ^:: nil ()
    in
    div ~a:(a_class (return classes) :: a) children

  let snackbar_label ?(classes = []) ?(a = []) ?label ?(children = nil ()) () =
    let classes = CSS.label :: classes in
    let label = Option.map (fun x -> return @@ txt x) label in
    div
      ~a:
        (a_class (return classes)
        :: a_aria "live" (return ["polite"])
        :: a_role (return ["status"])
        :: a)
      (label ^:: children)

  let snackbar_surface
      ?(classes = [])
      ?(a = [])
      ?label
      ?action
      ?dismiss
      ?actions
      ?children
      () =
    let classes = CSS.surface :: classes in
    let label =
      match label with
      | None -> None
      | Some (`Text s) -> Some (return @@ snackbar_label ~label:s ())
      | Some (`Element e) -> Some e
    in
    let actions =
      match actions, action, dismiss with
      | (Some _ as x), _, _ -> x
      | None, None, None -> None
      | None, Some _, _ | None, _, Some _ ->
          Some (return @@ snackbar_actions ?action ?dismiss ())
    in
    let children =
      match children with
      | Some x -> x
      | None -> label ^:: actions ^:: nil ()
    in
    div ~a:(a_class (return classes) :: a) children

  let snackbar
      ?(classes = [])
      ?(a = [])
      ?(leading = false)
      ?(stacked = false)
      ?dismiss
      ?action
      ?actions
      ?label
      ?surface
      ?children
      () =
    let classes =
      classes
      |> Utils.cons_if leading CSS.leading
      |> Utils.cons_if stacked CSS.stacked
      |> List.cons CSS.root
    in
    let children =
      match children with
      | Some x -> x
      | None -> (
        match surface with
        | Some x -> singleton x
        | None ->
            singleton (return (snackbar_surface ?action ?dismiss ?actions ?label ())))
    in
    div ~a:(a_class (return classes) :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
