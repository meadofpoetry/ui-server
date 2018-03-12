open Containers
open Components
open Wm_types
   
module Make(I : Item) = struct

  module IG = Wm_items_grid.Make(I)
  module RT = Wm_right_toolbar.Make(I)

  type t =
    { ig : IG.t
    ; lt : Box.t
    ; rt : RT.t
    }

  let make ~(title:       string)
           ~(init:        (string * I.item) list)
           ~(widgets:     (string * Wm.widget) list React.signal)
           ~(actions:     Fab.t list)
           ~(resolution:  (int * int))
           ~(cols:        int)
           ~(rows:        int)
           ~(s_conf:      editor_config React.signal)
           () =
    let rm = Wm_left_toolbar.make_action { icon = "delete"; name = "Удалить" } in

    let layers = I.layers_of_t_list init in
    (* fix layers indexes to be from 0 to n *)
    let init   = List.foldi (fun acc i x ->
                     List.map (fun (n,item) -> if I.layer_of_item item = x
                                               then n,I.update_layer item i
                                               else n,item) acc) init layers
    in

    let selected,selected_push = React.S.create None in
    (* FIXME bad desing, think how to remove this 'selected' signal *)
    let rt   = RT.make ~selected ~layers in
    let ig   = IG.make ~title ~resolution ~cols ~rows ~init ~selected_push
                       ~s_conf ~e_layers:rt#e_layers_action () in
    let lt   = Wm_left_toolbar.make (actions @ [rm]) in

    let _ = React.E.map (fun _ -> Option.iter (fun x -> x#remove) @@ React.S.value selected) rm#e_click in
    let _ = React.S.map (fun x -> rm#set_disabled @@ Option.is_none x) selected in
    { ig; lt; rt }

end
