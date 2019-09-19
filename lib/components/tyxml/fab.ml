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

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module CSS = CSS

  open Utils.Make (Xml)

  let fab ?(classes = []) ?(a = []) ?(mini = false) ?(extended = false) ?label ?icon () =
    let (classes : string list) =
      classes
      |> Utils.cons_if mini CSS.mini
      |> Utils.cons_if extended CSS.extended
      |> List.cons CSS.root
    in
    let label =
      match extended with
      | false -> None
      | true -> label
    in
    let label =
      match label with
      | None -> None
      | Some x ->
          Some
            (return
            @@ span ~a:[a_class (return [CSS.label])] (singleton (return (txt x))))
    in
    let content = icon ^:: label ^:: nil () in
    button ~a:(a_class (return classes) :: a) content
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
