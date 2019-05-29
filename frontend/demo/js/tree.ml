open Js_of_ocaml_tyxml
open Components

let make_node text children =
  let open Tyxml_js.Html in
  div ~a:[a_class ["mdc-treeview-node"; "mdc-treeview-node--click"]]
    [ div ~a:[a_class ["mdc-treeview-node__root"]]
        [ div ~a:[a_class ["mdc-treeview-node__content"]]
            [ div ~a:[a_class ["mdc-treeview-node__label"]]
                [txt text]
            ]
        ]
    ; div ~a:[a_class ["mdc-treeview-node__children"]]
        children
    ]

let make_leaf text =
  let open Tyxml_js.Html in
  div ~a:[a_class [ "mdc-treeview-node"
                  ; "mdc-treeview-node--leaf"
                  ; "mdc-treeview-node--click" ]]
    [ div ~a:[a_class ["mdc-treeview-node__root"]]
        [ div ~a:[a_class ["mdc-treeview-node__content"]]
            [ div ~a:[a_class ["mdc-treeview-node__label"]]
                [txt text]
            ]
        ]
    ]

let make_element () =
  let open Tyxml_js.Html in
  div ~a:[a_class ["mdc-treeview"]]
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

let make () =
  Widget.create (Tyxml_js.To_dom.of_element @@ make_element ())
