open Js_of_ocaml
open Application_types
open Components
open Topo_types

type node_entry = Topo_types.node_entry

let input_to_area ({ input; id } : Topology.topo_input) =
  Printf.sprintf "%s-%d" (Topology.input_to_string input) id

let board_to_area ({ manufacturer; model; version; control; _ } : Topology.topo_board) =
  Printf.sprintf "%s-%s-v%d-%d" manufacturer model version control

let node_entry_to_area = function
  | `CPU _ -> "CPU"
  | `Entry (Topology.Board b) -> board_to_area b
  | `Entry (Topology.Input i) -> input_to_area i

class t ~node ~body elt () =
  object
    val area = node_entry_to_area node
    inherit Widget.t elt ()
    method area : string = area
    method node_entry : node_entry = node
    method output_point = Topo_path.get_output_point body
  end

class parent ~port_setter
    ~(connections : (#t * connection_point) list)
    ~(node : node_entry)
    ~(body : #Dom_html.element Js.t)
    elt () =
  let num = List.length connections in
  let paths =
    List.mapi (fun i (x, p) ->
        let f_lp = fun () -> x#output_point in
        let f_rp = fun () -> Topo_path.get_input_point ~num i body in
        new Topo_path.t
          ~left_node:x#node_entry
          ~right_point:p
          ~f_lp ~f_rp ~port_setter ()) connections
  in
  let switches = Utils.List.filter_map (fun x -> x#switch) paths in
  let s_switch_changing =
    List.map (fun x -> x#forbidden) switches
    |> React.S.merge ~eq:(=)
      (fun acc x -> if x then x else acc) false
  in
  object
    val mutable _s = None
    inherit t ~node ~body elt () as super

    method! init () : unit =
      super#init ();
      let s = React.S.map ~eq:(=)
          (fun x -> List.iter (fun s -> s#set_forbidden x) switches)
          s_switch_changing in
      _s <- Some s

    method! layout () : unit =
      super#layout ();
      List.iter (fun p -> p#layout ()) paths

    method! destroy () : unit =
      super#destroy ();
      Option.iter (React.S.stop ~strong:true) _s;
      _s <- None

    method paths = paths

  end
