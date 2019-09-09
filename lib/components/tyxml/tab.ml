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
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Tab_indicator_markup = Tab_indicator.Make (Xml) (Svg) (Html)

  let create_text_label ?(classes = []) ?(attrs = []) ?label ?(content = []) () =
    let classes = CSS.text_label :: classes in
    span ~a:([a_class classes] @ attrs) (Utils.map_cons_option txt label content)

  let create_content ?(classes = []) ?(attrs = []) ?indicator ?icon ?text_label () =
    let classes = CSS.content :: classes in
    let text_label =
      match text_label with
      | None -> None
      | Some (`Text s) -> Some (create_text_label ~label:s ())
      | Some (`Element e) -> Some e
    in
    span ~a:([a_class classes] @ attrs) Utils.(icon ^:: text_label ^:: indicator ^:: [])

  let create_ripple ?(classes = []) ?(attrs = []) () =
    let classes = CSS.ripple :: classes in
    span ~a:([a_class classes] @ attrs) []

  let create
      ?(classes = [])
      ?(attrs = [])
      ?(active = false)
      ?(stacked = false)
      ?(disabled = false)
      ?(min_width = false)
      ?(indicator_span_content = false)
      ?indicator_icon
      ?icon
      ?text_label
      ?(ripple = create_ripple ())
      ?(indicator = Tab_indicator_markup.create ?icon:indicator_icon ())
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
      | Some x -> x
      | None ->
          create_content
            ?indicator:(if indicator_span_content then Some indicator else None)
            ?icon
            ?text_label
            ()
    in
    let children =
      match children with
      | Some x -> x
      | None ->
          if indicator_span_content
          then [content; ripple]
          else [content; indicator; ripple]
    in
    button
      ~a:
        ([a_class classes; a_role ["tab"]] @ attrs
        |> Utils.cons_if_lazy disabled a_disabled)
      children
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
