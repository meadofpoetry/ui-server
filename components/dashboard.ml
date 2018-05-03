open Containers
open Dynamic_grid

let base_class    = "mdc-dashboard"
let item_class    = Markup.CSS.add_element base_class "item"
let content_class = Markup.CSS.add_element item_class "content"
let heading_class = Markup.CSS.add_element item_class "heading"
let remove_class  = Markup.CSS.add_element item_class "remove-button"

module Position = Dynamic_grid.Position

module Item = struct

  type 'a settings =
    { widget : Widget.widget
    ; signal : 'a option React.signal
    ; set    : 'a -> (unit,string) Lwt_result.t
    }

  type 'a item =
    { name     : string
    ; settings : 'a settings option
    ; widget   : Widget.widget
    }

  type 'a positioned_item =
    { item     : 'a item
    ; position : Position.t
    }

  let to_item ?settings ~name (widget:#Widget.widget) =
    { name; settings; widget = widget#widget }

  let to_positioned_item ?settings ~name ~position (widget:#Widget.widget) =
    { item = to_item ?settings ~name widget; position }

  class t ~(item:'a item) () =
    let title   = new Card.Primary.title item.name () in
    let remove  = new Icon.Button.Font.t ~icon:"close" () in
    let heading = new Card.Primary.t ~widgets:[title#widget;remove#widget] () in
    let content = new Card.Media.t ~widgets:[item.widget] () in
    object(self)
      inherit Widget.widget (Dom_html.createDiv Dom_html.document) () as super
      method remove  = remove
      method content = content
      method heading = heading
      initializer
        self#add_class item_class;
        content#add_class content_class;
        heading#add_class heading_class;
        remove#add_class  remove_class;
        Dom.appendChild self#root heading#root;
        Dom.appendChild self#root content#root
    end

end

class ['a] t ~(items:'a Item.positioned_item list) () =
  let grid  = to_grid ~vertical_compact:true ~row_height:150 ~items_margin:(10,10) ~cols:4 () in
  object(self)
    inherit [unit] Dynamic_grid.t ~items:[] ~grid () as super
    method add_item (x:'a Item.positioned_item) =
      let db_item = new Item.t ~item:x.item () in
      let dg_item = Dynamic_grid.Item.to_item ~close_widget:db_item#remove#widget
                                              ~widget:db_item#widget
                                              ~value:()
                                              ~pos:x.position
                                              ()
      in
      super#add dg_item
      |> Result.map (fun _ -> db_item)

    initializer
      self#set_on_load @@ Some (fun () -> self#layout);
      self#add_class base_class;
      List.map self#add_item items |> ignore
  end
