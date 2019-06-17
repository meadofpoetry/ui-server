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
    { ig : Widget.t
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
    let layers = List.sort compare @@ I.layers_of_t_list init in
    let rt = RT.make ~selected ~layers ~candidates ~set_candidates in
    let ig = Layout_table.make () in
    { ig; rt }

end
