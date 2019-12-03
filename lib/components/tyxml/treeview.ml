module CSS = struct
  let root = "mdc-treeview"

  let node = root ^ "-node"

  let node_content = BEM.add_element node "content"

  let node_children = BEM.add_element node "children"

  let node_expander = BEM.add_element node "expander"

  let node_expanded = BEM.add_modifier node "expanded"

  let node_selected = BEM.add_modifier node "selected"

  let node_activated = BEM.add_modifier node "activated"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module Item_list_markup = Item_list.Make (Xml) (Svg) (Html)

  let ( % ) f g x = f (g x)

  let ( @:: ) = cons

  let ( ^:: ) x l = Option.fold ~none:l ~some:(fun x -> cons x l) x

  let treeview_children_wrapper ?(classes = return []) ?(a = []) ?(children = nil ()) ()
      =
    let classes = fmap (fun x -> CSS.node_children :: x) classes in
    ul ~a:(a_class classes :: a_role (return ["group"]) :: a) children

  let treeview_node_expander ?(classes = return []) ?(a = []) ?(children = nil ()) () =
    let classes =
      fmap (fun x -> CSS.node_expander :: Item_list.CSS.item_meta :: x) classes
    in
    span ~a:(a_class classes :: a) children

  let treeview_node_content
      ?(classes = return [])
      ?(a = [])
      ?graphic
      ?meta
      ?role
      ?primary_text
      ?secondary_text
      ?force_wrap
      ?children
      () =
    let classes = fmap (fun x -> Item_list.CSS.item :: CSS.node_content :: x) classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          let text =
            Item_list_markup.list_item_text ?force_wrap ?primary_text ?secondary_text ()
          in
          graphic ^:: return text @:: meta ^:: nil ()
    in
    span
      ~a:
        (a_class classes :: a
        |> Utils.map_cons_option (fun x -> a_role (return [x])) role)
      children

  let treeview_node
      ?(classes = return [])
      ?(a = [])
      ?value
      ?level
      ?(expanded = false)
      ?(tabindex = -1)
      ?(selected = false)
      ?(checked = false)
      ?(indeterminate = false)
      ?graphic
      ?meta
      ?role
      ?primary_text
      ?secondary_text
      ?force_wrap
      ?node_content
      ?child_nodes
      ?children_wrapper
      () : 'a elt =
    let node_content =
      match node_content with
      | Some x -> return x
      | None ->
          let meta =
            match meta, children_wrapper, child_nodes with
            | Some x, _, _ -> Some (return x)
            | None, None, None -> None
            | None, Some _, _ | None, _, Some _ ->
                Some (return (treeview_node_expander ()))
          in
          return
            (treeview_node_content
               ?graphic
               ?meta
               ?role
               ?primary_text
               ?secondary_text
               ?force_wrap
               ())
    in
    let checked =
      if indeterminate then "mixed" else if checked then "true" else "false"
    in
    let children_wrapper =
      match children_wrapper with
      | Some x -> Some (return x)
      | None -> (
        match child_nodes with
        | None -> None
        | Some nodes -> Some (return (treeview_children_wrapper ~children:nodes ())))
    in
    let classes = fmap (fun x -> CSS.node :: x) classes in
    li
      ~a:
        (a_class classes
         :: a_role (return ["treeitem"])
         :: a_aria "selected" (return [string_of_bool selected])
         :: a_aria "checked" (return [checked])
         :: a_aria "expanded" (return [string_of_bool expanded])
         :: a_tabindex (return tabindex)
         :: a
        |> Utils.map_cons_option
             (fun x -> a_aria "level" (return [string_of_int x]))
             level
        |> Utils.map_cons_option (a_user_data "value") value)
      (node_content @:: children_wrapper ^:: nil ())

  let treeview
      ?(classes = return [])
      ?(a = [])
      ?multiselectable
      ?(dense = false)
      ?(two_line = false)
      ?(children = nil ())
      () =
    let classes =
      fmap
        (Utils.cons_if dense Item_list.CSS.dense (* FIXME *)
        % Utils.cons_if two_line Item_list.CSS.two_line (* FIXME *)
        % List.cons CSS.root)
        classes
    in
    ul
      ~a:
        (a_class classes :: a_role (return ["tree"]) :: a
        |> Utils.map_cons_option
             (fun x ->
               let v = string_of_bool x in
               a_aria "multiselectable" (return [v]))
             multiselectable)
      children
end
