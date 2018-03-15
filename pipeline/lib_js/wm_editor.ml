open Containers
open Components
open Wm_types

let remove ~eq cs set (t:'a wm_item) =
  if t.unique then set @@ t :: (List.filter (fun x -> not @@ eq t x) cs)

module Make(I : Item) = struct

  module IG = Wm_items_grid.Make(I)
  module RT = Wm_right_toolbar.Make(I)

  type t =
    { ig : IG.t
    ; lt : Box.t
    ; rt : RT.t
    }

  let make ?(on_remove:      (I.t -> unit) option)
           ~(title:          string)
           ~(resolution:     (int * int))
           ~(init:           I.t list)
           ~(candidates:     I.t list React.signal)
           ~(set_candidates: I.t list -> unit)
           ~(actions:        Fab.t list)
           () =
    let rm = Wm_left_toolbar.make_action { icon = "delete"; name = "Удалить" } in

    let layers = I.layers_of_t_list init in
    (* fix layers indexes to be from 0 to n *)
    let init   = List.foldi (fun acc i layer ->
                     List.map (fun x -> if I.layer_of_t x = layer
                                        then I.update_layer x i
                                        else x) acc) init layers
    in
    let selected,selected_push = React.S.create None in
    (* FIXME bad desing, think how to remove this 'selected' signal *)
    let rt   = RT.make ~selected ~layers ~candidates ~set_candidates in
    let ig   = IG.make ~title ~resolution ~init ~selected_push ~e_layers:rt#e_layers_action () in
    let lt   = Wm_left_toolbar.make (rm :: actions) in

    let _ = React.S.diff (fun n o ->
                let eq = fun x1 x2 -> Equal.physical x1#root x2#root in
                let rm = List.filter_map (fun x -> if not @@ List.mem ~eq x n
                                                   then Some (List.map (fun x -> x#get_value) x#items)
                                                   else None) o
                         |> List.flatten in
                List.iter (fun x -> remove ~eq:I.equal (React.S.value candidates) set_candidates x) rm)
                         ig#s_layers in
    let _ = React.E.map (fun _ -> Option.iter (fun x ->
                                      Option.iter (fun f -> f x#get_value) on_remove;
                                      remove ~eq:I.equal (React.S.value candidates) set_candidates x#get_value;
                                      x#remove)
                                  @@ React.S.value selected)
                        rm#e_click in
    let _ = React.S.map (fun x -> rm#set_disabled @@ Option.is_none x) selected in
    { ig; lt; rt }

end
