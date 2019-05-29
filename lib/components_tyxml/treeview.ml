module CSS = struct
  let root = "mdc-treeview"

  let node = root ^ "-node"

  let node_disabled = BEM.add_modifier node "disabled"

end
