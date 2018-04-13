open Common.Topology
open Containers
open Components

let _class = "topology"

let find_max l =
  let rec f = (fun max l ->
      (match l with
       | [] -> max
       | hd :: tl -> f (if hd > max then hd else max) tl)) in
  (match l with
   | [] -> failwith "find max: list is empty"
   | hd :: tl -> f hd tl)

(*calculates the depth of the node*)
let rec get_entry_depth depth = function
  | Input _ -> succ depth
  | Board x -> (let ports = List.map (fun x -> x.child) x.ports in
                match ports with
                | [] -> succ depth
                | l  -> succ @@ find_max (List.map (get_entry_depth depth) l))

let get_node_depth t =
  let depth = 0 in
  match t with
  | `CPU x    -> succ @@ find_max (List.map (fun x -> get_entry_depth depth x.conn) x.ifaces)
  | `Boards x -> succ @@ find_max
                           (List.map (fun x ->
                                succ @@ find_max
                                          (List.map (fun x -> get_entry_depth 0 x.child) x.ports)
                              ) x)

let rm_children container =
  Dom.list_of_nodeList @@ container##.childNodes
  |> List.iter (fun x -> Dom.removeChild container x)

let concat (l : string list) : string =
  List.fold_left (fun acc x -> x^" "^acc) "" l

let grid_template_areas t =
  let depth = get_node_depth t in
  let rec get_entry_areas acc count = function
    | Input x -> if (count+1) < depth
                 then let inp  = "\""^(Topo_node.input_to_area x) in
                      let list = CCList.range 1 @@ (depth-count-1)*2 in
                      (List.fold_left (fun acc _ -> acc^" . ") inp list)^acc^"\""
                 else "\""^(Topo_node.input_to_area x)^" "^acc^"\""
    | Board x -> (let ports = List.map (fun x -> x.child) x.ports in
                  let str   = ". "^(Topo_node.board_to_area x)^" " in
                  match ports with
                  | [] -> str^" "^acc
                  | l  -> concat (List.map (get_entry_areas (str^acc) (count+1)) l))
  in
  match t with
  | `CPU x    -> concat (List.map (fun x -> get_entry_areas ". CPU" 1 x.conn) x.ifaces)
  | `Boards x -> concat (List.map (fun board ->
                             concat (List.map (fun x ->
                                         get_entry_areas (". "^(Topo_node.board_to_area board)) 1 x.child)
                                              board.ports)) x)

let wrap area elt =
  let div = Dom_html.createDiv Dom_html.document |> Widget.create in
  div#add_class @@ Markup.CSS.add_element _class "node-wrapper";
  div#style##.cssText := Js.string @@ "grid-area: "^area^";";
  Dom.appendChild div#root elt#root;
  div

let to_topo_node = function
  | `Board x -> (x :> Topo_node.t)
  | `Input x -> (x :> Topo_node.t)
  | `CPU x   -> (x :> Topo_node.t)

let make_nodes topology =
  let create_element ~(element:Topo_node.node_entry) ~connections =
    let connections = List.map (fun (x,p) -> to_topo_node x, p) connections in
    match element with
    | `Entry (Board board) -> `Board (Topo_board.create ~connections board)
    | `Entry (Input input) -> `Input (Topo_input.create input)
    | `CPU cpu             -> `CPU   (Topo_cpu.create cpu ~connections)
  in
  let rec get_boards acc = function
    | Input _ as i -> let i = create_element ~element:(`Entry i) ~connections:[] in
                      i,i::acc
    | Board x as b -> let connections,acc = match x.ports with
                        | [] -> [],acc
                        | l  -> List.fold_left (fun (conn,total) x ->
                                    let e,acc = get_boards acc x.child in
                                    (e, `Port x)::conn,acc@total) ([],[]) l
                      in
                      let b = create_element ~element:(`Entry b) ~connections in
                      b,b::acc
  in
  match topology with
  | `CPU cpu  -> let connections,acc = List.fold_left (fun (conn,total) x ->
                                           let e,acc = get_boards [] x.conn in
                                           (e, `Iface x)::conn,acc@total) ([],[]) cpu.ifaces in
                 let cpu_el = create_element ~element:(`CPU cpu) ~connections in
                 cpu_el::acc
  | `Boards x -> List.map (fun board ->
                     let connections,acc = List.fold_left (fun (conn,total) x ->
                                               let e,acc = get_boards [] x.child in
                                               (e, `Port x)::conn,acc@total) ([],[]) board.ports in
                     let b = create_element ~element:(`Entry (Board board)) ~connections in
                     b :: acc) x
                 |> List.flatten

let iter_paths f nodes =
  List.iter (function
             | `Board b -> List.iter (fun p -> f (b :> Topo_node.t) p) b#paths
             | `CPU c   -> List.iter (fun p -> f (c :> Topo_node.t) p) c#paths
             | _        -> ()) nodes

let update_nodes nodes (t:Common.Topology.t) =
  let boards = Common.Topology.boards t in
  let f (b:Topo_board.t) (x:topo_board) = Topo_board.eq_board b#board x in
  List.iter (function
             | `Board b -> (match List.find_opt (f b) boards with
                            | Some tb -> b#set_board tb
                            | None    -> ())
             | _        -> ()) nodes

let create ~(parent: #Widget.widget)
           ~(init:   Common.Topology.t)
           ~(event:  Common.Topology.t React.event)
           () =
  let svg    = Tyxml_js.Svg.(svg ~a:[a_class [Markup.CSS.add_element _class "paths"]] [] |> toelt) in
  let nodes  = make_nodes init in
  let e_s    = List.filter_map (function `Board b -> Some (React.E.map (fun x -> `Board x) b#e_settings)
                                       | `CPU c   -> Some (React.E.map (fun x -> `CPU x) c#e_settings)
                                       | _        -> None) nodes
               |> React.E.select
  in
  let drawer,drawer_box,set_drawer_title = Topo_drawer.make ~title:"" () in
  let _      =
    React.E.map (fun node ->
        rm_children drawer_box#root;
        let error_prefix = "Ошибка при загрузке страницы" in
        let res = match node with
          | `Board board ->
             set_drawer_title @@ Topo_board.get_board_name board;
             Topo_board.make_board_page ~error_prefix board
          | `CPU cpu ->
             set_drawer_title @@ Topo_cpu.get_cpu_name cpu;
             Topo_cpu.make_cpu_page ~error_prefix cpu
        in
        let pgs = Ui_templates.Progress.create_progress_block_lwt ~error_prefix ~get:fst res in
        Dom.appendChild drawer_box#root pgs#root;
        Lwt.Infix.(drawer#show_await
                   >>= (fun () -> Lwt_result.bind res (fun (_,f) -> Lwt_result.return @@ f ())))
        |> ignore) e_s
  in
  iter_paths (fun _ x -> Option.iter (fun sw -> Dom.appendChild parent#root sw#root) x#switch;
                         Dom.appendChild svg x#root) nodes;
  Dom.appendChild Dom_html.document##.body drawer#root;
  Dom.appendChild parent#root svg;
  List.iter (fun x -> let node = to_topo_node x in
                      let w    = wrap node#area node in
                      Dom.appendChild parent#root w#root) nodes;
  let gta = "grid-template-areas: " ^ (grid_template_areas init) ^ ";" in
  let _   = React.E.map (update_nodes nodes) event in
  parent#style##.cssText := Js.string gta;
  nodes
