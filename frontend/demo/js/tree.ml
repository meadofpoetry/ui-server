open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

let make_node text children =
  let open Tyxml_js.Html in
  let graphic = Checkbox.make () in
  let meta = span ~a:[a_class [ "mdc-treeview-node__expand-icon"
                              ; Item_list.CSS.item_meta ]] [] in
  graphic#add_class Item_list.CSS.item_graphic;
  div ~a:[a_class ["mdc-treeview-node"]]
    [ div ~a:[a_class ["mdc-treeview-node__root"]]
        [ div ~a:[ a_class ["mdc-treeview-node__content"; "mdc-list-item"]
                 ; a_role ["checkbox"]]
            [ graphic#markup
            ; div ~a:[a_class ["mdc-list-item__text"]] [txt text]
            ; meta
            ]
        ]
    ; div ~a:[a_class ["mdc-treeview-node__children"]]
        children
    ]

let make_leaf text =
  let open Tyxml_js.Html in
  let graphic = Checkbox.make () in
  graphic#add_class Item_list.CSS.item_graphic;
  div ~a:[a_class [ "mdc-treeview-node"
                  ; "mdc-treeview-node--leaf" ]]
    [ div ~a:[a_class ["mdc-treeview-node__root"]]
        [ div ~a:[ a_class ["mdc-treeview-node__content"; "mdc-list-item"]
                 ; a_role ["checkbox"] ]
            [ graphic#markup
            ; div ~a:[a_class ["mdc-list-item__text"]] [txt text]
            ]
        ]
    ]

let make_element () =
  let open Tyxml_js.Html in
  div ~a:[a_class ["mdc-treeview"; "mdc-list--dense"]]
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

let log : 'a -> unit = fun x ->
  Js.Unsafe.global##.console##log x


let make () =
  let w = Tyxml_js.To_dom.of_element @@ make_element () in
  let w = new Treeview.t w () in
  w#root##.style##.maxWidth := Utils.px_js 400;
  log w#root;
  (* (match Element.query_selector w#root ".mdc-treeview-node" with
   *  | None -> print_endline "no root found"
   *  | Some x ->
   *    print_endline "all children: ";
   *    let children = w#get_node_children x in
   *    (match children with
   *     | [] -> print_endline "no children found"
   *     | x -> List.iter log x);
   *    let leafs = w#get_all_descendant_leafs x in
   *    (match leafs with
   *     | [] -> print_endline "no leafs found"
   *     | x -> List.iter log x)); *)
  w
