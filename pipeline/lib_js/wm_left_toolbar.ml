open Containers
open Components
open Wm_types

let base_class   = "wm-left-toolbar"
let action_class = Markup.CSS.add_element base_class "action"

let make_action (action : action) =
  let w  = new Fab.t ~mini:true ~icon:action.icon () in
  let () = w#add_class action_class in
  let () = w#set_attribute "title" action.name in
  w

let make widgets =
  let box = new Box.t ~widgets () in
  let ()  = box#add_class base_class in
  box
