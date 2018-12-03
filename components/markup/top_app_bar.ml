open Utils
open Containers

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  (** Mandatory. *)
  let base_class = "mcd-top-app-bar"

  (** Class used to style the top app bar as a fixed top app bar. *)
  let fixed_class = CSS.add_modifier base_class "fixed"

  (** Class used to style the content below the standard and fixed
      top app bar to prevent the top app bar from covering it. *)
  let fixed_adjust_class = CSS.add_modifier base_class "fixed-adjust"

  (** Class used to style the top app bar as a prominent top app bar. *)
  let prominent = CSS.add_modifier base_class "prominent"

  (** Class used to style the content below the prominent top app bar
      to prevent the top app bar from covering it. *)
  let prominent_fixed_adjust_class =
    CSS.add_modifier base_class "prominent-fixed-adjust"

  (** Class used to style the top app bar as a dense top app bar. *)
  let dense_class = CSS.add_modifier base_class "dense"

  (** Class used to style the content below the dense top app bar
      to prevent the top app bar from covering it. *)
  let dense_fixed_adjust_class =
    CSS.add_modifier base_class "dense-fixed-adjust"

  (** Class used to style the content below the top app bar when
      styled as both prominent and dense, to prevent the top app bar
      from covering it. *)
  let dense_prominent_fixed_adjust_class =
    CSS.add_modifier base_class "dense-prominent-fixed-adjust"

  (** Class used to style the top app bar as a short top app bar. *)
  let short_class = CSS.add_modifier base_class "short"

  (** Class used to indicate the short top app bar is collapsed. *)
  let short_collapsed_class = CSS.add_modifier base_class "short-collapsed"

  (** Class used to style the content below the short top app bar
      to prevent the top app bar from covering it. *)
  let short_fixed_adjust_class =
    CSS.add_modifier base_class "short-fixed-adjust"

  let row_class = CSS.add_element base_class "row"

  let section_class = CSS.add_element base_class "section"

  let section_align_start_class = CSS.add_modifier section_class "align-start"

  let section_align_end_class = CSS.add_modifier section_class "align-end"

  let action_item_class = CSS.add_element base_class "action-item"

  let navigation_icon_class = CSS.add_element base_class "navigation-icon"

  let title_class = CSS.add_element base_class "title"

  let create_title ?(classes = []) ?attrs ~content () =
    span ~a:([a_class (title_class :: classes)] <@> attrs) content

  let create_section ?(classes = []) ?attrs ?align ~content () =
    let classes = match align with
      | None -> classes
      | Some `Start -> section_align_start_class :: classes
      | Some `End -> section_align_end_class :: classes in
    section ~a:([a_class (section_class :: classes)] <@> attrs) content

  let create_row ?(classes = []) ?attrs ~sections () =
    div ~a:([a_class (row_class :: classes)] <@> attrs) sections

  let create ?(classes = []) ?attrs ~rows () =
    header ~a:([a_class (base_class :: classes)] <@> attrs) rows

end

