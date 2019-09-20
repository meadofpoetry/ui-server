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
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  let ( % ) f g x = f (g x)

  let ( @:: ) = cons

  let ( ^:: ) x l = Option.fold ~none:l ~some:(fun x -> cons x l) x

  let top_app_bar_title ?(classes = return []) ?(a = []) ?title ?(children = nil ()) () =
    let classes = fmap (fun x -> CSS.title :: x) classes in
    let children =
      match title with
      | None -> children
      | Some x -> cons (return (txt x)) children
    in
    span ~a:(a_class classes :: a) children

  let top_app_bar_section ?(classes = return []) ?(a = []) ?align ?(children = nil ()) ()
      =
    let align_class =
      match align with
      | None -> None
      | Some `Start -> Some CSS.section_align_start
      | Some `End -> Some CSS.section_align_end
    in
    let classes = fmap (Utils.cons_option align_class % List.cons CSS.section) classes in
    section ~a:(a_class classes :: a) children

  let top_app_bar_row ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes = fmap (fun x -> CSS.row :: x) classes in
    div ~a:(a_class classes :: a) children

  let top_app_bar ?(classes = return []) ?(a = []) ?leading ?title ?actions ?children ()
      =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let title =
            match title with
            | None -> None
            | Some (`Element x) -> Some (return x)
            | Some (`Text x) -> Some (return (top_app_bar_title ~title:x ()))
          in
          let start_section =
            return
              (top_app_bar_section
                 ~align:`Start
                 ~children:(leading ^:: title ^:: nil ())
                 ())
          in
          let end_section =
            match actions with
            | None -> None
            | Some x -> Some (return (top_app_bar_section ~align:`End ~children:x ()))
          in
          let sections = start_section @:: end_section ^:: nil () in
          singleton (return (top_app_bar_row ~children:sections ()))
    in
    header ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
