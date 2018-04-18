open Containers
open Components
open Dynamic_grid

let base_class    = "mdc-widget-grid"
let wrapper_class = Markup.CSS.add_element base_class "widget-wrapper"
let widget_class  = Markup.CSS.add_element base_class "widget"

module Item = struct

  let to_item widget =
    let wrapper = Widget.create (Dom_html.createDiv Dom_html.document) in
    let ()      = Dom.appendChild wrapper#root widget#root in
    let ()      = widget#add_class widget_class in
    let ()      = wrapper#add_class wrapper_class in
    Item.to_item ~widget:wrapper ~value:() ()

end

class t ~items () =
  let grid = to_grid ~vertical_compact:true ~row_height:100 ~items_margin:(10,10) ~cols:4 () in
  object(self)

    inherit [unit] Dynamic_grid.t ~items ~grid ()
    initializer
      self#set_on_load @@ Some (fun () -> self#layout);
      self#add_class base_class

  end
