open Containers
open Components
open Common.Topology
open Topo_types

type node_entry = Topo_types.node_entry

let input_to_area ({ input; id }:topo_input) =
  let str = string_of_int id in
  (match input with
   | RF    -> "RF"
   | TSOIP -> "TSOIP"
   | ASI   -> "ASI")^str

let board_to_area (board : topo_board) =
  let str = string_of_int board.control in
  board.typ ^ str

let node_entry_to_area = function
  | `CPU _           -> "CPU"
  | `Entry (Board b) -> board_to_area b
  | `Entry (Input i) -> input_to_area i

class t ~node ~body elt () =
object
  val area = node_entry_to_area node
  inherit Widget.widget elt ()
  method area : string     = area
  method node : node_entry = node
  method layout : unit     = ()
  method output_point      = Topo_path.get_output_point body
end

class parent ~(connections:(#t * connection_point) list)
             ~(node:node_entry)
             ~(body:#Dom_html.element Js.t)
             elt
             () =
  let num = List.length connections in
  let cw  = List.mapi (fun i (x,p) -> let f_lp = fun () -> x#output_point in
                                      let f_rp = fun () -> Topo_path.get_input_point ~num i body in
                                      new Topo_path.t
                                          ~left_node:x#node
                                          ~right_point:p
                                          ~f_lp ~f_rp ()) connections
  in
  object(self)
    inherit t ~node ~body elt () as super
    method layout = super#layout; List.iter (fun p -> p#layout) self#paths
    method paths : Topo_path.t list = cw
  end
