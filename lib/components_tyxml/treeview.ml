module CSS = struct
  let root = "mdc-treeview"

  let node = root ^ "-node"

  let node_root = BEM.add_element node "root"

  let node_leaf = BEM.add_modifier node "leaf"

  let node_content = BEM.add_element node "content"

  let node_children = BEM.add_element node "children"

end
