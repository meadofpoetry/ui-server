open Containers
open Dynamic_grid

include Dashboard_common
include Dashboard_grid

module Position = Dynamic_grid.Position
module Item     = Dashboard_item

let list_to_yojson f l = `List (List.map f l)
let list_of_yojson f = function
  | `List l -> List.map f l |> List.all_ok
  | _       -> Error "not a list"

class ['a] t ~(items:'a Item.positioned_item list) (factory:'a #factory) () =
  let grid = new grid factory () in
  let add_panel =
    new Dashboard_panel.add
      ~widgets:(List.map (fun x -> new Dashboard_add_item.t x ())
                  (match factory#available with
                   | `List l -> l
                   | `Groups _ -> [])) ()
  in
  let add_icon  = Icon.SVG.(new t ~paths:Path.[ new t plus () ] ()) in
  let edit_icon = Icon.SVG.(new t ~paths:Path.[ new t pencil () ] ()) in
  let add = new Fab.t ~icon:add_icon () in
  let fab = new Fab_speed_dial.t
              ~direction:`Up
              ~animation:`Scale
              ~icon:edit_icon
              ~items:[add] () in
  let e, push = React.E.create () in
  object(self)

    inherit Vbox.t ~widgets:[ grid#widget; fab#widget ] ()

    method e_edited : Yojson.Safe.json React.event = e

    method grid = grid

    method serialize () : Yojson.Safe.json =
      List.map (fun x ->
          ({ position = x#pos; item = fst x#value} : 'a Item.positioned_item))
        self#grid#items
      |> list_to_yojson (Item.positioned_item_to_yojson factory#serialize)

    method deserialize (json:Yojson.Safe.json) : ('a Item.positioned_item list,string) result =
      list_of_yojson (fun x -> Item.positioned_item_of_yojson factory#deserialize x) json
    method restore (json:Yojson.Safe.json) : (unit,string) result =
      self#deserialize json
      |> Result.map (fun l -> List.iter (fun x -> x#remove ()) self#grid#items; (* remove previous items *)
                              List.iter (fun x -> self#grid#add x |> ignore) l)

    method destroy () = factory#destroy (); Dom.removeChild Dom_html.document##.body add_panel#root

    initializer
      fab#main#listen Widget.Event.click (fun _ _ ->
          (match React.S.value fab#s_state with
           | false -> fab#show ()
           | true  -> push @@ self#serialize ();
                      fab#hide ());
          true)|> ignore;
      add#listen Widget.Event.click (fun _ _ ->
          add_panel#show (); true) |> ignore;
      React.S.map (function
          | true  -> grid#set_editable true;
                     edit_icon#path#set Icon.SVG.Path.check
          | false -> grid#set_editable false;
                     edit_icon#path#set Icon.SVG.Path.pencil)
        fab#s_state |> ignore;
      Dom.appendChild Dom_html.document##.body add_panel#root;
      self#set_on_load @@ Some (fun () -> self#grid#layout (); fab#hide ());
      fab#add_class Markup.edit_button_class;
      self#add_class Markup.base_class;
      List.map self#grid#add items |> ignore;
      self#grid#set_editable false;
  end
