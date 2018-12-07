open Containers
open Components
open Js_of_ocaml
open Topo_types
open Common

type node_entry = Topo_types.node_entry

let input_to_area ({ input; id } : Topology.topo_input) =
  let str = string_of_int id in
  (match input with
   | RF -> "RF"
   | TSOIP -> "TSOIP"
   | ASI   -> "ASI")^str

let board_to_area (board : Topology.topo_board) =
  let str = string_of_int board.control in
  board.typ ^ str

let node_entry_to_area = function
  | `CPU _ -> "CPU"
  | `Entry (Topology.Board b) -> board_to_area b
  | `Entry (Topology.Input i) -> input_to_area i

class t ~node ~body elt () =
object
  val area = node_entry_to_area node
  inherit Widget.t elt ()
  method area : string     = area
  method node_entry : node_entry = node
  method layout () : unit  = ()
  method output_point = Topo_path.get_output_point body
end

class parent ~port_setter
        ~(connections : (#t * connection_point) list)
        ~(node : node_entry)
        ~(body : #Dom_html.element Js.t)
        elt () =
  let num = List.length connections in
  let paths =
    List.mapi (fun i (x,p) ->
        let f_lp = fun () -> x#output_point in
        let f_rp = fun () -> Topo_path.get_input_point ~num i body in
        new Topo_path.t
          ~left_node:x#node_entry
          ~right_node:node
          ~right_point:p
          ~f_lp ~f_rp ~port_setter ()) connections
  in
  let switches = List.filter_map (fun x -> x#switch) paths in
  let s_switch_changing =
    List.map (fun x -> x#s_changing) switches
    |> React.S.merge ~eq:Bool.equal
         (fun acc x -> if x then x else acc) false
  in
  object(self)
    inherit t ~node ~body elt () as super

    method init () : unit =
      super#init ();
      React.S.map ~eq:Equal.unit
        (fun x -> List.iter (fun s -> s#set_changing x) switches)
        s_switch_changing
      |> self#_keep_s;

    method layout () : unit =
      super#layout ();
      List.iter (fun p -> p#layout ()) paths

    method paths = paths

  end
