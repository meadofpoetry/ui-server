module CSS = struct
  let root = "mdc-fab-speed-dial"

  let main = BEM.add_element root "main"
  let action = BEM.add_element root "action"
  let actions = BEM.add_element root "actions"

  let fling = BEM.add_modifier root "animation-fling"
  let scale = BEM.add_modifier root "animation-scale"

  let up = BEM.add_modifier root "direction-up"
  let down = BEM.add_modifier root "direction-down"
  let left = BEM.add_modifier root "direction-left"
  let right = BEM.add_modifier root "direction-right"

  let opened = BEM.add_modifier root "opened"
end
