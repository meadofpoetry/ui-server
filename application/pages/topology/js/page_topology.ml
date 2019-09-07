open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Application_types
open Netlib

module CSS = struct
  let root = "topology"

  let paths = BEM.add_element root "paths"

  let non_interactive = BEM.add_modifier root "non-interactive"
end

let is_ts2ip_niitv (b : Topology.topo_board) =
  match b.manufacturer, b.model with
  | "NIITV", "TS2IP" -> true
  | _ -> false

let find_max l =
  let rec f max = function
    | [] -> max
    | hd :: tl -> f (if hd > max then hd else max) tl
  in
  match l with
  | [] -> failwith "find max: list is empty"
  | hd :: tl -> f hd tl

(* calculates the depth of the node *)
let rec get_entry_depth depth = function
  | Topology.Input _ -> succ depth
  | Topology.Board x -> (
      let ports = List.map (fun (x : Topology.topo_port) -> x.child) x.ports in
      match ports with
      | [] -> succ depth
      | l -> succ @@ find_max (List.map (get_entry_depth depth) l))

let get_node_depth t =
  let depth = 0 in
  match t with
  | `CPU (x : Topology.topo_cpu) ->
      succ
      @@ find_max
           (List.map
              (fun (x : Topology.topo_interface) -> get_entry_depth depth x.conn)
              x.ifaces)
  | `Boards x ->
      find_max
        (List.map
           (fun (x : Topology.topo_board) ->
             succ
             @@ find_max
                  (List.map
                     (fun (x : Topology.topo_port) -> get_entry_depth 0 x.child)
                     x.ports))
           x)
      |> succ

let concat (l : string list) : string = List.fold_left (fun acc x -> x ^ " " ^ acc) "" l

let grid_template_areas t =
  let depth = get_node_depth t in
  let rec get_entry_areas acc count = function
    | Topology.Input x ->
        if count + 1 < depth
        then
          let inp = "\"" ^ Topo_node.input_to_area x in
          let rec aux acc = function
            | 0 -> acc
            | n -> aux (acc ^ " . ") (pred n)
          in
          aux inp (2 * (depth - count - 1)) ^ acc ^ "\""
        else "\"" ^ Topo_node.input_to_area x ^ " " ^ acc ^ "\""
    | Topology.Board x -> (
        let ports = List.map (fun (x : Topology.topo_port) -> x.child) x.ports in
        let str = ". " ^ Topo_node.board_to_area x ^ " " in
        match ports with
        | [] -> str ^ " " ^ acc
        | l -> concat (List.map (get_entry_areas (str ^ acc) (count + 1)) l))
  in
  match t with
  | `CPU (x : Topology.topo_cpu) ->
      List.map
        (fun (x : Topology.topo_interface) -> get_entry_areas ". CPU" 1 x.conn)
        x.ifaces
      |> concat
  | `Boards x ->
      let map board = get_entry_areas (". " ^ Topo_node.board_to_area board) 1 in
      List.map
        (fun board ->
          List.map (fun (x : Topology.topo_port) -> map board x.child) board.ports
          |> concat)
        x
      |> concat

let wrap area elt =
  let div = Widget.create_div () in
  div#add_class @@ BEM.add_element CSS.root "node-wrapper";
  div#root##.style##.cssText := Js_of_ocaml.Js.string @@ "grid-area: " ^ area ^ ";";
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
    | iface :: rest -> (
      match iface.conn with
      | Input _ -> aux (iface :: acc) rest
      | Board b -> (
        match is_ts2ip_niitv b with
        | false -> aux (iface :: acc) rest
        | true ->
            let ifaces =
              List.rev_map (fun x -> {iface = iface.iface; conn = x.child}) b.ports
            in
            aux (acc @ ifaces) rest))
  in
  let ifaces = List.rev @@ aux [] cpu.ifaces in
  {cpu with ifaces}

let make_nodes topology socket =
  let open Topology in
  let create_element ~(element : Topo_node.node_entry) ~connections =
    let connections = List.map (fun (x, p) -> to_topo_node x, p) connections in
    match element with
    | `Entry (Board board) -> `Board (Topo_board.create ~connections socket board)
    | `Entry (Input input) -> `Input (Topo_input.create input)
    | `CPU cpu -> `CPU (Topo_cpu.create ~connections socket cpu)
  in
  let rec get_boards acc = function
    | Input _ as i ->
        let i = create_element ~element:(`Entry i) ~connections:[] in
        i, i :: acc
    | Board x as b ->
        let connections, acc =
          match x.ports with
          | [] -> [], acc
          | l ->
              List.fold_left
                (fun (conn, total) x ->
                  let e, acc = get_boards acc x.child in
                  (e, `Port x) :: conn, acc @ total)
                ([], [])
                l
        in
        let b = create_element ~element:(`Entry b) ~connections in
        b, b :: acc
  in
  match topology with
  | `CPU cpu ->
      let cpu' = map_cpu_conn cpu in
      let connections, acc =
        List.fold_left
          (fun (conn, total) x ->
            let e, acc = get_boards [] x.conn in
            (e, `Iface x) :: conn, acc @ total)
          ([], [])
          cpu'.ifaces
      in
      let cpu_el = create_element ~element:(`CPU cpu) ~connections in
      cpu_el :: acc
  | `Boards x ->
      List.map
        (fun board ->
          let connections, acc =
            List.fold_left
              (fun (conn, total) x ->
                let e, acc = get_boards [] x.child in
                (e, `Port x) :: conn, acc @ total)
              ([], [])
              board.ports
          in
          let b = create_element ~element:(`Entry (Board board)) ~connections in
          b :: acc)
        x
      |> List.flatten

let iter_paths f nodes =
  List.iter
    (function
      | `Board b -> List.iter (fun p -> f (b :> Topo_node.t) p) b#paths
      | `CPU c -> List.iter (fun p -> f (c :> Topo_node.t) p) c#paths
      | `Input _ -> ())
    nodes

let update_nodes nodes (t : Topology.t) =
  let boards = Topology.get_boards t in
  let eq (b : Topo_board.t) = Topo_board.eq_board b#board in
  List.iter
    (function
      | `CPU _ | `Input _ -> ()
      | `Board b -> (
        match List.find_opt (eq b) boards with
        | Some tb -> b#notify (`State tb)
        | None -> ()))
    nodes

type event = [`Topology of Topology.t]

let create (init : Topology.t) (socket : Api_js.Websocket.JSON.t) =
  let svg =
    Tyxml_js.Svg.(
      svg ~a:[a_class [CSS.paths]] [] |> toelt |> Js.Unsafe.coerce |> Widget.create)
  in
  let nodes = make_nodes init socket in
  let gta = Printf.sprintf "grid-template-areas: %s;" (grid_template_areas init) in
  let e_settings =
    List.filter_map
      (function
        | `Board (b : Topo_board.t) -> Some b#settings_event
        | `CPU (c : Topo_cpu.t) -> Some c#settings_event
        | `Input _ -> None)
      nodes
    |> React.E.select
  in
  object (self)
    val mutable _resize_observer = None

    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      let username = Js.to_string @@ Js.Unsafe.global##.username in
      let non_interactive =
        match User.of_string username with
        | Ok `Guest | Error _ -> true
        | Ok (`Operator | `Root) -> false
      in
      super#add_class Layout_grid.CSS.inner;
      if non_interactive then super#add_class CSS.non_interactive;
      iter_paths
        (fun _ x ->
          Option.iter super#append_child x#switch;
          svg#append_child x)
        nodes;
      super#append_child svg;
      List.iter
        (fun x ->
          let node = to_topo_node x in
          let w = wrap node#area node in
          super#append_child w)
        nodes;
      super#root##.style##.cssText := Js.string gta;
      let obs =
        Resize_observer.observe ~node:super#root ~f:(fun _ _ -> self#layout ()) ()
      in
      _resize_observer <- Some obs

    method! destroy () : unit =
      super#destroy ();
      Option.iter (fun x -> x##disconnect) _resize_observer;
      _resize_observer <- None

    method! layout () : unit =
      super#layout ();
      List.iter
        (function
          | `Board b -> b#layout ()
          | `Input i -> i#layout ()
          | `CPU c -> c#layout ())
        nodes

    method notify : event -> unit =
      function
      | `Topology x -> update_nodes nodes x

    method e_settings = e_settings
  end

let ( >>= ) = Lwt.( >>= )

let ( >>=? ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let on_settings
    (side_sheet : #Side_sheet.Parent.t)
    (content : #Widget.t)
    (set_title : string -> unit)
    cur
    old =
  (match old with
  | Some (w, _) -> w#destroy ()
  | None -> ());
  content#remove_children ();
  match cur with
  | None -> Lwt.return_unit
  | Some (widget, name) ->
      content#remove_children ();
      content#append_child widget;
      set_title name;
      side_sheet#toggle ~force:true ()

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let side_sheet, side_sheet_content, set_side_sheet_title =
    Topo_drawer.make ~title:"" ()
  in
  let on_settings = on_settings side_sheet side_sheet_content set_side_sheet_title in
  let thread =
    Application_http_js.get_topology ()
    >>=? fun init ->
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>=? fun socket ->
    Application_http_js.Event.get_topology socket
    >>=? fun (_, event) ->
    let page = create init socket in
    let event = React.E.map (fun x -> page#notify (`Topology x)) event in
    let s_settings =
      React.S.hold None @@ React.E.map (fun x -> Some x) page#e_settings
    in
    let close =
      Lwt_js_events.seq_loop
        (Lwt_js_events.make_event Side_sheet.Event.close)
        side_sheet#root
        (fun _ _ ->
          match React.S.value s_settings with
          | None -> Lwt.return_unit
          | Some (w, _) ->
              w#destroy ();
              Lwt.return_unit)
    in
    page#set_on_destroy (fun () ->
        Lwt.cancel close;
        React.E.stop ~strong:true event;
        Api_js.Websocket.close_socket socket);
    Lwt_react.(E.keep @@ S.diff_s on_settings s_settings);
    Lwt.return_ok page
  in
  let body = Components_lab.Loader.make_widget_loader thread in
  Element.add_class body CSS.root;
  Element.add_class body Layout_grid.CSS.root;
  scaffold#set_side_sheet ~elevation:Full_height side_sheet;
  scaffold#set_body body
