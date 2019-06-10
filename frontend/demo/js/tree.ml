open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

let make_node text children =
  let graphic = Checkbox.make () in
  graphic#add_class Item_list.CSS.item_graphic;
  Treeview.Markup.create_node ~graphic:graphic#markup ~children text

let make_leaf text =
  let graphic = Checkbox.make () in
  graphic#add_class Item_list.CSS.item_graphic;
  Treeview.Markup.create_node ~graphic:graphic#markup text

let make_element () =
  Treeview.Markup.create ~dense:true
    [ make_node "Tree Root"
        [ make_node "Root node"
            [ make_node "Level 2 node"
                [ make_leaf "Leaf node 1"
                ; make_leaf "Leaf node 2"
                ; make_leaf "Leaf node 3"
                ]
            ]
        ; make_node "Second root"
            [ make_leaf "Leaf node 1"
            ; make_leaf "Leaf node 2"
            ; make_leaf "Leaf node 3"
            ]
        ]
    ]

let log : 'a -> unit = fun x ->
  Js.Unsafe.global##.console##log x

let make () =
  let w = Tyxml_js.To_dom.of_element @@ make_element () in
  let w = new Treeview.t w () in
  w#root##.style##.maxWidth := Utils.px_js 400;
  List.iter (fun x -> print_endline @@ Treeview.show_node Format.pp_print_string x) w#value;
  w
