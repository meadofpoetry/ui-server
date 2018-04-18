open Containers
open Components
open Dynamic_grid

let base_class    = "mdc-widget-grid"
let widget_class  = Markup.CSS.add_element base_class "widget"
let content_class = Markup.CSS.add_element widget_class "content"
let heading_class = Markup.CSS.add_element widget_class "heading"
let close_class   = Markup.CSS.add_element widget_class "close"

module Item = struct

  type 'a settings = Widget.widget * 'a option React.signal * ('a -> (unit,string) Lwt_result.t)

  class type ['a] t =
    object
      inherit Widget.widget
      method name     : string
      method set_name : string -> unit
      method settings : 'a settings option
    end

  class ['a] wrapper (widget:'a t) () =
    let title = new Card.Primary.title widget#name () in
    let close = new Icon.Button.Font.t ~icon:"close" () in
    let prim  = new Card.Primary.t ~widgets:[title#widget;close#widget] () in
    let media = new Card.Media.t ~widgets:[widget] () in
    object(self)
      inherit Widget.widget (Dom_html.createDiv Dom_html.document) () as super
      method! layout = super#layout; widget#layout
      method close   = close
      initializer
        self#add_class widget_class;
        media#add_class content_class;
        prim#add_class heading_class;
        close#add_class close_class;
        Dom.appendChild self#root prim#root;
        Dom.appendChild self#root media#root
    end

  let to_item (widget:'a t) =
    let wrapper = new wrapper widget () in
    Item.to_item ~close_widget:wrapper#close#widget ~widget:wrapper#widget ~value:() ()

end

class t ~items () =
  let grid = to_grid ~vertical_compact:true ~row_height:150 ~items_margin:(10,10) ~cols:4 () in
  object(self)

    inherit [unit] Dynamic_grid.t ~items ~grid ()
    initializer
      self#set_on_load @@ Some (fun () -> self#layout);
      self#add_class base_class

  end
