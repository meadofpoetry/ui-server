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
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create_text_label ?(classes = []) ?attrs text () : 'a elt =
    let classes = CSS.text_label :: classes in
    span ~a:([a_class classes] <@> attrs) [txt text]

  let create_content ?(classes = []) ?attrs
        ?indicator ?icon ?text_label () : 'a elt =
    let classes = CSS.content :: classes in
    span ~a:([a_class classes] <@> attrs)
      (icon ^:: text_label ^:: indicator ^:: [])

  let create ?(classes = []) ?attrs ?(active = false) ?(stacked = false)
        ?(disabled = false) ?(min_width = false) ?indicator content () : 'a elt =
    let classes =
      classes
      |> cons_if active CSS.active
      |> cons_if stacked CSS.stacked
      |> cons_if min_width CSS.min_width
      |> List.cons CSS.root in
    button ~a:([ a_class classes
               ; a_role ["tab"] ]
               |> cons_if_lazy disabled a_disabled
               <@> attrs)
      (content :: (indicator ^:: (span ~a:[a_class [CSS.ripple]] [] :: [])))
end
