open Components
open Wm_types

let ( % ) f g x = f (g x)

let remove ~eq cs set (t : 'a wm_item) =
  if t.unique then
    set @@ t :: (List.filter (not % eq t) @@ React.S.value cs)

module Make(I : Item) = struct

  module IG = Grid.Make(I)
  module RT = Controls.Make(I)

  type t =
    { ig : IG.t
    ; rt : RT.t
    }

  let make ?(on_remove : (I.t -> unit) option)
           ~(title : string)
           ~(resolution : (int * int))
           ~(init : I.t list)
           ~(candidates : I.t list React.signal)
           ~(set_candidates : I.t list -> unit)
           () =
    let selected, selected_push = React.S.create None in
    let e_click, set_click = React.E.create () in
    let layers = List.sort compare @@ I.layers_of_t_list init in
    (* fix layers indexes to be from 0 to n *)
    let init =
      fst
      @@ List.fold_left (fun (acc, i) layer ->
          let acc =
            List.map (fun x -> if I.layer_of_t x = layer
                       then I.update_layer x i
                       else x) acc in
          acc, succ i) (init, 0) layers in
    let rt = RT.make ~selected ~layers ~candidates ~set_candidates in
    let ig = IG.make ~title ~resolution ~init ~e_layers:rt#e_layers_action () in

    let _ = React.S.map selected_push ig#s_selected in
    let _ = React.S.diff (fun n o ->
                let eq = Widget.equal in
                let rm =
                  List.flatten
                  @@ Utils.List.filter_map (fun x ->
                      if not @@ List.exists (eq x) n
                      then Some (List.map (fun x -> x#value) x#items)
                      else None) o in
                List.iter (fun x -> remove ~eq:I.equal candidates set_candidates x) rm)
              ig#s_layers in
    (* FIXME store event *)
    let _ =
      React.E.map (fun _ ->
          Utils.Option.iter (fun x ->
              Utils.Option.iter (fun f -> f x#value) on_remove;
              remove ~eq:I.equal candidates set_candidates x#value;
              x#remove ())
          @@ React.S.value selected)
      @@ React.E.select [ig#e_item_delete; e_click] in
    (* FIXME *)
    (* let _ = React.S.map (fun x -> rm#set_disabled @@ Option.is_none x) selected in *)
    { ig; rt }

end
