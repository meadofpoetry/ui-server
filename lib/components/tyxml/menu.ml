module CSS = struct
  (** Required on the root element. *)
  let root = "mdc-menu"

  (** Used to wrap a group of mdc-list-item elements that will represent
      a selection group. *)
  let selection_group = BEM.add_element root "selection-group"

  (** Required when using a selection group to indicate which item is
      selected. Should contain an icon or svg that indicates the selected
      state of the list item. *)
  let selection_group_icon = BEM.add_element root "selection-group-icon"

  (** Used to indicate which element in a selection group is selected. *)
  let item_selected = BEM.add_modifier "mdc-menu-item" "selected"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module Menu_surface_markup = Menu_surface.Make (Xml) (Svg) (Html)

  module Item_list = struct
    include Item_list.Make (Xml) (Svg) (Html)

    let list_item = list_item ~role:"menuitem"

    let list = list ~role:"menu"
  end

  let menu
      ?(classes = return [])
      ?(a = [])
      ?fixed
      ?open_
      ?list_children
      ?list
      ?children
      () =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    let a = a_tabindex (return (-1)) :: a in
    let children =
      match children with
      | Some _ as x -> x
      | None -> (
        match list with
        | Some x -> Some (singleton (return x))
        | None -> (
          match list_children with
          | None -> None
          | Some x -> Some (singleton (return (Item_list.list ~children:x ())))))
    in
    Menu_surface_markup.menu_surface ~classes ~a ?fixed ?open_ ?children ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
