module CSS = struct
  (** Mandatory, for the button element. *)
  let root = "mdc-fab"

  (** Mandatory, for the icon element. *)
  let icon = BEM.add_element root "icon"

  (** Optional, for the text label. Applicable only for Extended FAB. *)
  let label = BEM.add_element root "label"

  (** Optional, animates the FAB out of view.
      When this class is removed, the FAB will return to view. *)
  let exited = BEM.add_modifier root "exited"

  (** Optional, modifies the FAB to a smaller size. *)
  let mini = BEM.add_modifier root "mini"

  (** Optional, modifies the FAB to wider size which includes a text label. *)
  let extended = BEM.add_modifier root "extended"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  open Html
  open Utils

  let create ?(classes = []) ?(attrs = []) ?(mini = false) ?(extended = false)
        ?label ?icon () : 'a elt =
    let (classes : string list) =
      classes
      |> cons_if mini CSS.mini
      |> cons_if extended CSS.extended
      |> List.cons CSS.root in
    let label = match extended with
      | false -> None
      | true -> label in
    let label = match label with
      | None -> None
      | Some x -> Some (span ~a:[a_class [CSS.label]] [txt x]) in
    let content = icon ^:: label ^:: [] in
    button ~a:([a_class classes] @ attrs) content
end
