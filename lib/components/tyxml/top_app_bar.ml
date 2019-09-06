module CSS = struct
  (** Mandatory. *)
  let root = "mdc-top-app-bar"

  (** Class used to style the top app bar as a fixed top app bar. *)
  let fixed = BEM.add_modifier root "fixed"

  (** Class used to style the content below the standard and fixed
      top app bar to prevent the top app bar from covering it. *)
  let fixed_adjust = BEM.add_modifier root "fixed-adjust"

  (** Class used to style the top app bar as a prominent top app bar. *)
  let prominent = BEM.add_modifier root "prominent"

  (** Class used to style the content below the prominent top app bar
      to prevent the top app bar from covering it. *)
  let prominent_fixed_adjust = BEM.add_modifier root "prominent-fixed-adjust"

  (** Class used to style the top app bar as a dense top app bar. *)
  let dense = BEM.add_modifier root "dense"

  (** Class used to style the content below the dense top app bar
      to prevent the top app bar from covering it. *)
  let dense_fixed_adjust = BEM.add_modifier root "dense-fixed-adjust"

  (** Class used to style the content below the top app bar when
      styled as both prominent and dense, to prevent the top app bar
      from covering it. *)
  let dense_prominent_fixed_adjust = BEM.add_modifier root "dense-prominent-fixed-adjust"

  (** Class used to style the top app bar as a short top app bar. *)
  let short = BEM.add_modifier root "short"

  (** Class used to indicate the short top app bar is collapsed. *)
  let short_collapsed = BEM.add_modifier root "short-collapsed"

  (** Class used to style the content below the short top app bar
      to prevent the top app bar from covering it. *)
  let short_fixed_adjust = BEM.add_modifier root "short-fixed-adjust"

  let fixed_scrolled = fixed ^ "-scrolled"

  let row = BEM.add_element root "row"

  let section = BEM.add_element root "section"

  let section_align_start = BEM.add_modifier section "align-start"

  let section_align_end = BEM.add_modifier section "align-end"

  let action_item = BEM.add_element root "action-item"

  let navigation_icon = BEM.add_element root "navigation-icon"

  let title = BEM.add_element root "title"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html

  let create_title ?(classes = []) ?(attrs = []) ~content () : 'a elt =
    span ~a:([a_class (CSS.title :: classes)] @ attrs) content

  let create_section ?(classes = []) ?(attrs = []) ?align ~content () : 'a elt =
    let classes =
      match align with
      | None -> classes
      | Some `Start -> CSS.section_align_start :: classes
      | Some `End -> CSS.section_align_end :: classes
    in
    section ~a:([a_class (CSS.section :: classes)] @ attrs) content

  let create_row ?(classes = []) ?(attrs = []) ~sections () : 'a elt =
    div ~a:([a_class (CSS.row :: classes)] @ attrs) sections

  let create ?(classes = []) ?(attrs = []) ~rows () : 'a elt =
    header ~a:([a_class (CSS.root :: classes)] @ attrs) rows
end
