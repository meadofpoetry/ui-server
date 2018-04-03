open Containers
open Components
open Common.Topology

type point =
  { x : int
  ; y : int
  }

let get_output_point widget =
  let rect : Widget.rect = widget#get_bounding_client_rect in
  let y = (int_of_float rect.top) + (widget#get_offset_height / 2) in
  { x = int_of_float rect.left; y }

class node_with_output ~(entry:topo_entry) (connectable:#Widget.widget) =
  let s_op,s_op_push = React.S.create (get_output_point connectable) in
  object

    method output_point = s_op
    method entry        = entry

    initializer
      Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ ->
                          s_op_push (get_output_point connectable); true)
      |> ignore

  end

class t ~(connections:#node_with_output list)
        (widget:#Widget.widget)
        () =
object

end
