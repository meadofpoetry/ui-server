open Js_of_ocaml
open Components

let make () =
  let grid =
    { Dynamic_grid.
      min_col_width = 20
    ; max_col_width = None
    ; cols = 30
    ; rows = None
    ; row_height = Some 20
    ; vertical_compact = true
    ; items_margin = 5, 5
    ; multi_select = false
    ; restrict_move = false
    ; draggable = Some true
    ; resizable = Some true
    ; selectable = Some true
    } in
  let items = Dynamic_grid.Item.(
      [ to_item ~value:() ~pos:{ x = 1; y = 1; w = 4; h = 4 } ()
      ; to_item ~value:() ~pos:{ x = 5; y = 5; w = 4; h = 4 } ~resizable:false ()
      ]) in
  let t = new Dynamic_grid.t ~items ~grid () in
  let text = Typography.Text.make "Dynamic Grid" in
  Widget.create_div ~widgets:[text#widget; t#widget] ()
