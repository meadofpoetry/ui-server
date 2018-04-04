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
  { x = int_of_float rect.right; y }

let connect ~left ~right () =
  ()

class node_with_output ~(entry:topo_entry) (connectable:#Widget.widget) =
  let s_op,s_op_push = React.S.create (get_output_point connectable) in
  object

    method output_point = get_output_point connectable

  end

class t ~(connections:#node_with_output list)
        (widget:#Widget.widget)
        () =
object(self)
  method private connect_ =
    let rect : Widget.rect = widget#get_bounding_client_rect in
    let n = List.length connections in
    let h = widget#get_offset_height / n in
    List.iteri (fun i node -> let x     = int_of_float rect.left in
                              let y     = (i * h) + (h / 2) in
                              let right = { x; y } in
                              let left  = node#output_point in
                              connect ~left ~right ())
      connections
  initializer
    Dom_events.(listen Dom_html.window Typ.resize (fun _ _ -> self#connect_; true)) |> ignore
end
