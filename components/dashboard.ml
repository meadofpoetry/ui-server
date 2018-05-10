open Containers
open Dynamic_grid
open Dashboard_common

include Dashboard_grid

module Position = Dynamic_grid.Position
module Item     = Dashboard_item

let edit_button_class = Markup.CSS.add_element base_class "edit-button"

type 'a lst = 'a list [@@deriving yojson]

class ['a] t ~(items:'a Item.positioned_item list) (factory:'a #factory) () =
  let grid = new grid factory () in
  let add_panel = new Dashboard_panel.add ~widgets:(List.map (fun x -> new Dashboard_add_item.t x ())
                                                             (match factory#available with
                                                              | `List l -> l
                                                              | `Groups _ -> [])) ()
  in
  let fab = new Fab.t ~icon:"edit" () in
  let _   = React.E.map (fun _ -> grid#set_editable @@ not grid#editable;
                                  add_panel#show ()) fab#e_click in
  object(self)

    inherit Box.t ~vertical:true ~widgets:[grid#widget;fab#widget] ()

    method grid = grid

    method serialize () : Yojson.Safe.json =
      List.map (fun x -> let (i:'a Item.positioned_item) = { position = x#pos; item = fst x#value} in
                         Item.positioned_item_to_yojson factory#serialize i) self#grid#items
      |> fun l -> `List l

    method deserialize (json:Yojson.Safe.json) : ('a Item.positioned_item list,string) result =
      lst_of_yojson (fun x -> Item.positioned_item_of_yojson factory#deserialize x) json
    method restore (json:Yojson.Safe.json) : (unit,string) result =
      self#deserialize json
      |> Result.map (fun l -> List.iter (fun x -> x#remove ()) self#grid#items; (* remove previous items *)
                              List.iter (fun x -> self#grid#add x |> ignore) l)

    method destroy () = factory#destroy (); Dom.removeChild Dom_html.document##.body add_panel#root

    initializer
      Dom.appendChild Dom_html.document##.body add_panel#root;
      self#set_on_load @@ Some (fun () -> self#grid#layout ());
      fab#add_class edit_button_class;
      self#add_class base_class;
      List.map self#grid#add items |> ignore;
      self#grid#set_editable false;
  end
