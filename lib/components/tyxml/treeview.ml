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
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Item_list_markup = Item_list.Make (Xml) (Svg) (Html)

  let create_children ?(classes = []) ?(attrs = []) ?(nodes = []) () : 'a elt =
    let classes = CSS.node_children :: classes in
    ul ~a:([a_class classes; a_role ["group"]] @ attrs) nodes

  let create_node_expander ?(classes = []) ?(attrs = []) () =
    let classes = CSS.node_expander :: Item_list.CSS.item_meta :: classes in
    span ~a:([a_class classes] @ attrs) []

  let create_node_content
      ?(classes = [])
      ?(attrs = [])
      ?graphic
      ?meta
      ?role
      ?primary_text
      ?secondary_text
      ?force_wrap
      ?(content =
        Item_list_markup.create_item_text ?force_wrap ?primary_text ?secondary_text ())
      () : 'a elt =
    let classes =
      classes |> List.cons Item_list.CSS.item |> List.cons CSS.node_content
    in
    span
      ~a:([a_class classes] @ attrs |> Utils.map_cons_option (fun x -> a_role [x]) role)
      Utils.(graphic ^:: (content :: (meta ^:: [])))

  let create_node
      ?(classes = [])
      ?(attrs = [])
      ?value
      ?level
      ?child_nodes
      ?children
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
      ?content
      () : 'a elt =
    let content =
      match content with
      | Some x -> x
      | None ->
          let meta =
            match meta, children, child_nodes with
            | (Some _ as x), _, _ -> x
            | None, None, Some [] | None, None, None -> None
            | None, Some _, _ | None, _, Some _ -> Some (create_node_expander ())
          in
          create_node_content
            ?graphic
            ?meta
            ?role
            ?primary_text
            ?secondary_text
            ?force_wrap
            ()
    in
    let checked =
      if indeterminate then "mixed" else if checked then "true" else "false"
    in
    let children =
      match children with
      | Some _ as x -> x
      | None -> (
        match child_nodes with
        | None | Some [] -> None
        | Some nodes -> Some (create_children ~nodes ()))
    in
    let classes = CSS.node :: classes in
    li
      ~a:
        ([ a_class classes
         ; a_role ["treeitem"]
         ; a_aria "selected" [string_of_bool selected]
         ; a_aria "checked" [checked]
         ; a_aria "expanded" [string_of_bool expanded]
         ; a_tabindex tabindex ]
         @ attrs
        |> Utils.map_cons_option (fun x -> a_aria "level" [string_of_int x]) level
        |> Utils.map_cons_option (a_user_data "value") value)
      Utils.(content :: (children ^:: []))

  let create
      ?(classes = [])
      ?(attrs = [])
      ?multiselectable
      ?(dense = false)
      ?(two_line = false)
      nodes =
    let classes =
      classes
      |> Utils.cons_if dense Item_list.CSS.dense (* FIXME *)
      |> Utils.cons_if two_line Item_list.CSS.two_line (* FIXME *)
      |> List.cons CSS.root
    in
    ul
      ~a:
        ([a_class classes; a_role ["tree"]] @ attrs
        |> Utils.map_cons_option
             (fun x ->
               let v = string_of_bool x in
               a_aria "multiselectable" [v])
             multiselectable)
      nodes
end
