open Js_of_ocaml
open Containers
open Components
open Common

let _class = "topology"

let is_ts2ip_niitv (b : Topology.topo_board) =
  match b.typ, b.model, b.manufacturer with
  | "TS2IP", "ts2ip", "niitv" -> true
  | _ -> false

let find_max l =
  let rec f = fun max ->
    function
    | [] -> max
    | hd :: tl -> f (if hd > max then hd else max) tl in
  match l with
  | [] -> failwith "find max: list is empty"
  | hd :: tl -> f hd tl

(* calculates the depth of the node *)
let rec get_entry_depth depth = function
  | Topology.Input _ -> succ depth
  | Topology.Board x ->
     let ports = List.map (fun (x : Topology.topo_port) ->
                     x.child) x.ports in
     begin match ports with
     | [] -> succ depth
     | l -> succ @@ find_max (List.map (get_entry_depth depth) l)
     end

let get_node_depth t =
  let depth = 0 in
  match t with
  | `CPU (x : Topology.topo_cpu) ->
     succ @@ find_max (List.map (fun (x : Topology.topo_interface) ->
                           get_entry_depth depth x.conn) x.ifaces)
  | `Boards x ->
     find_max
       (List.map (fun (x : Topology.topo_board) ->
            succ @@ find_max
                      (List.map (fun (x : Topology.topo_port) ->
                           get_entry_depth 0 x.child) x.ports)
          ) x)
     |> succ

let grid_template_areas t =
  let depth = get_node_depth t in
  let concat = List.fold_left (fun acc x -> x ^ " " ^ acc) "" in
  let rec get_entry_areas acc count = function
    | Topology.Input x ->
       if (count + 1) < depth
       then let inp  = "\"" ^ (Topo_node.input_to_area x) in
            let list = List.range 1 @@ (depth - count - 1) * 2 in
            (List.fold_left (fun acc _ -> acc ^ " . ") inp list) ^ acc ^ "\""
       else "\"" ^ (Topo_node.input_to_area x) ^ " " ^ acc ^ "\""
    | Topology.Board x ->
       let ports = List.map (fun (x : Topology.topo_port) ->
                       x.child) x.ports in
       let str = ". " ^ (Topo_node.board_to_area x) ^ " " in
       match ports with
       | [] -> str ^ " " ^ acc
       | l -> concat @@ List.map (get_entry_areas (str ^ acc) (count + 1)) l in
  match t with
  | `CPU (x : Topology.topo_cpu) ->
     concat @@ List.map (fun (x : Topology.topo_interface) ->
                   get_entry_areas ". CPU" 1 x.conn) x.ifaces
  | `Boards x ->
     let map (b : Topology.topo_board) (p : Topology.topo_port) : string =
       get_entry_areas (". " ^ (Topo_node.board_to_area b)) 1 p.child in
     concat @@ List.map (fun b -> concat @@ List.map (map b) b.ports) x

let wrap area elt =
  let div = Widget.create_div () in
  div#add_class @@ Markup.CSS.add_element _class "node-wrapper";
  div#style##.cssText := Js_of_ocaml.Js.string @@ "grid-area: " ^ area ^ ";";
  div#append_child elt;
  div

let to_topo_node = function
  | `Board x -> (x :> Topo_node.t)
  | `Input x -> (x :> Topo_node.t)
  | `CPU x -> (x :> Topo_node.t)

let map_cpu_conn (cpu : Topology.topo_cpu) : Topology.topo_cpu =
  let open Topology in
  let rec aux acc = function
    | [] -> acc
    | iface :: rest ->
       match iface.conn with
       | Input _ -> aux (iface :: acc) rest
       | Board b ->
          match is_ts2ip_niitv b with
          | false -> aux (iface :: acc) rest
          | true ->
             let ifaces =
               List.rev_map (fun x ->
                   { iface = iface.iface
                   ; conn = x.child }) b.ports in
             aux (acc @ ifaces) rest in
  let ifaces = List.rev @@ aux [] cpu.ifaces in
  { cpu with ifaces }

let make_nodes topology =
  let open Topology in
  let create_element ~(element : Topo_node.node_entry) ~connections =
    let connections = List.map (fun (x,p) -> to_topo_node x, p) connections in
    match element with
    | `Entry (Board board) -> `Board (Topo_board.create ~connections board)
    | `Entry (Input input) -> `Input (Topo_input.create input)
    | `CPU cpu -> `CPU (Topo_cpu.create cpu ~connections) in
  let rec get_boards acc = function
    | Input _ as i ->
       let i = create_element ~element:(`Entry i) ~connections:[] in
       i, i :: acc
    | Board x as b ->
       let connections, acc = match x.ports with
         | [] -> [], acc
         | l  -> List.fold_left (fun (conn,total) x ->
                     let e, acc = get_boards acc x.child in
                     (e, `Port x) :: conn, acc @ total) ([], []) l
       in
       let b = create_element ~element:(`Entry b) ~connections in
       b, b :: acc
  in
  match topology with
  | `CPU cpu  ->
     let cpu' = map_cpu_conn cpu in
     let connections, acc =
       List.fold_left (fun (conn, total) x ->
           let e, acc = get_boards [] x.conn in
           (e, `Iface x) :: conn, acc @ total) ([], []) cpu'.ifaces in
     let cpu_el = create_element ~element:(`CPU cpu) ~connections in
     cpu_el :: acc
  | `Boards x ->
     List.flat_map (fun board ->
         let connections, acc =
           List.fold_left (fun (conn, total) x ->
               let e, acc = get_boards [] x.child in
               (e, `Port x) :: conn, acc @ total) ([], []) board.ports in
         let b = create_element ~element:(`Entry (Board board)) ~connections in
         b :: acc) x

let iter_paths f nodes =
  List.iter (function
      | `Board b -> List.iter (fun p -> f (b :> Topo_node.t) p) b#paths
      | `CPU c -> List.iter (fun p -> f (c :> Topo_node.t) p) c#paths
      | _ -> ()) nodes

let update_nodes nodes (t : Topology.t) =
  let boards = Topology.get_boards t in
  let f (b : Topo_board.t) (x : Topology.topo_board) =
    Topo_board.eq_board b#board x in
  List.iter (function
      | `Board b ->
         begin match List.find_opt (f b) boards with
         | Some tb -> b#set_board tb
         | None -> ()
         end
      | _ -> ()) nodes

let create (init : Topology.t) =
  let event, sock = Requests.WS.get_topology () in
  let svg = Tyxml_js.Svg.(
      svg ~a:[a_class [Markup.CSS.add_element _class "paths"]] []
      |> toelt
      |> Js.Unsafe.coerce
      |> Widget.create) in
  let nodes = make_nodes init in
  let gta =
    Printf.sprintf "grid-template-areas: %s;"
      (grid_template_areas init) in
  let e_settings =
    List.filter_map (function
        | `Board (b : Topo_board.t) -> Some b#settings_event
        | `CPU (c : Topo_cpu.t) -> Some c#settings_event
        | `Input _ -> None) nodes
    |> React.E.select in
  object(self)
    val mutable e_upd = None
    val mutable _resize_observer = None

    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#add_class Layout_grid.Markup.inner_class;
      super#add_class _class;
      iter_paths (fun _ x ->
          Option.iter super#append_child x#switch;
          svg#append_child x) nodes;
      super#append_child svg;
      List.iter (fun x ->
          let node = to_topo_node x in
          let w = wrap node#area node in
          super#append_child w) nodes;
      super#style##.cssText := Js.string gta;
      let e' = React.E.map (update_nodes nodes) event in
      e_upd <- Some e';
      let obs =
        Ui_templates.Resize_observer.observe ~node:super#root
          ~f:(fun a ->
            let rect =
              Js.array_get a 0
              |> Js.Optdef.to_option
              |> Option.get_exn
              |> (fun e -> e##.contentRect) in
            Js.Unsafe.global##.console##log rect |> ignore;
            self#layout ()) () in
      _resize_observer <- Some obs;

    method! destroy () : unit =
      super#destroy ();
      React.E.stop ~strong:true event;
      sock##close;
      Option.iter (React.E.stop ~strong:true) e_upd;
      e_upd <- None;
      Option.iter Ui_templates.Resize_observer.disconnect _resize_observer;
      _resize_observer <- None

    method! layout () : unit =
      super#layout ();
      List.iter (function
          | `Board b -> b#layout ()
          | `Input i -> i#layout ()
          | `CPU c -> c#layout ()) nodes

    method e_settings = e_settings

  end
