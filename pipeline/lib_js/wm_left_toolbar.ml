open Components

let base_class = "wm-left-toolbar"
let action_class = CSS.add_element base_class "action"

let make_action (action : Wm_types.action) =
  let w = new Fab.t ~mini:true ~icon:action.icon () in
  w#add_class action_class;
  w#set_attribute "title" action.name;
  w

let make widgets =
  let box = new Vbox.t ~widgets () in
  box#add_class base_class;
  box
