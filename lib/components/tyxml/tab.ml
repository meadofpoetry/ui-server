module CSS = struct
  (** Mandatory. *)
  let root = "mdc-tab"

  (** Mandatory. Indicates the content of the tab. *)
  let content = BEM.add_element root "content"

  (** Optional. Indicates a leading icon in the tab. *)
  let icon = BEM.add_element root "icon"

  (** Optional. Indicates a text label of the tab. *)
  let text_label = BEM.add_element root "text-label"

  (** Mandatory. Denotes the ripple surface for the tab. *)
  let ripple = BEM.add_element root "ripple"

  (** Optional. Indicates that the tab is active. *)
  let active = BEM.add_modifier root "active"

  (** Optional. Indicates that the tab icon and label should flow vertically
      instead of horizontally. *)
  let stacked = BEM.add_modifier root "stacked"

  (** Optional. Indicates that the tab should shrink in size to be as narrow
      as possible without causing text to wrap. *)
  let min_width = BEM.add_modifier root "min-width"
end

module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module CSS = CSS
  module Tab_indicator = Tab_indicator.Make (Xml) (Svg) (Html)

  open Utils.Make (Xml)

  let tab_text_label ?(classes = []) ?(a = []) ?label ?(children = nil ()) () =
    let classes = CSS.text_label :: classes in
    let children =
      match label with
      | None -> children
      | Some x -> cons (return (txt x)) children
    in
    span ~a:(a_class (return classes) :: a) children

  let tab_content ?(classes = []) ?(a = []) ?indicator ?icon ?text_label () =
    let classes = CSS.content :: classes in
    let text_label =
      match text_label with
      | None -> None
      | Some (`Text s) -> Some (return @@ tab_text_label ~label:s ())
      | Some (`Element e) -> Some (return e)
    in
    span ~a:(a_class (return classes) :: a) (icon ^:: text_label ^:: indicator ^:: nil ())

  let tab_ripple ?(classes = []) ?(a = []) ?(children = nil ()) () =
    let classes = CSS.ripple :: classes in
    span ~a:(a_class (return classes) :: a) children

  let tab
      ?(classes = [])
      ?(a = [])
      ?(active = false)
      ?(stacked = false)
      ?(disabled = false)
      ?(min_width = false)
      ?(indicator_span_content = false)
      ?indicator_icon
      ?icon
      ?text_label
      ?(ripple = tab_ripple ())
      ?(indicator = Tab_indicator.tab_indicator ?icon:indicator_icon ())
      ?content
      ?children
      () =
    let classes =
      classes
      |> Utils.cons_if active CSS.active
      |> Utils.cons_if stacked CSS.stacked
      |> Utils.cons_if min_width CSS.min_width
      |> List.cons CSS.root
    in
    let content =
      match content with
      | Some x -> return x
      | None ->
          return
          @@ tab_content
               ?indicator:
                 (if indicator_span_content then Some (return indicator) else None)
               ?icon
               ?text_label
               ()
    in
    let children =
      match children with
      | Some x -> x
      | None ->
          if indicator_span_content
          then content @:: return ripple @:: nil ()
          else content @:: return indicator @:: return ripple @:: nil ()
    in
    button
      ~a:
        (a_class (return classes) :: a_role (return ["tab"]) :: a
        |> Utils.cons_if_lazy disabled a_disabled)
      children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
