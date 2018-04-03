[@@@ocaml.warning "-60"]

open Js_of_ocaml
open Common.Topology
open Containers
open Tyxml_js
open Components

(*All positioning constants in GG and VG mean: *)
(*X from left to right and Y from bottom to top*)
(*    0.0,1.0 __________________  1.0,1.0      *)
(*           |                  |              *)
(*           |                  |              *)
(*           |                  |              *)
(*    0.0,0.0|__________________| 1.0,0.0      *)

let input_to_string ({ input; id }:topo_input) =
  let id = string_of_int id in
  (match input with
   | RF    -> "RF "
   | TSOIP -> "TSoIP "
   | ASI   -> "ASI ")^id

let board_to_string (tp:board_type) =
  match tp with
  | "DVB"   -> "DVB-T/T2/C"
  | "TS"    -> "QoS"
  | "IP2TS" -> "TSoIP"
  | "TS2IP" -> "QoE"
  | s       -> failwith ("unknown board " ^ s)

let input_to_area ({ input; id }:topo_input) =
  let str = string_of_int id in
  (match input with
   | RF    -> "RF"
   | TSOIP -> "TSOIP"
   | ASI   -> "ASI")^str

let board_to_area (board : topo_board) =
  let str = string_of_int board.control in
  board.typ ^ str

let floor_to_five x =
  let fx = floor x +. 0.5 in
  if Float.(x > fx) then fx else floor x

let ceil_to_five x =
  let cx = ceil x -. 0.5 in
  if Float.(x < cx) then cx else ceil x

let find_max l =
  let rec f = (fun max l ->
      (match l with
       | [] -> max
       | hd :: tl -> f (if hd > max then hd else max) tl)) in
  (match l with
   | [] -> failwith "find max: list is empty"
   | hd :: tl -> f hd tl)

let rec get_entry_height height = function
      Input _ -> succ height
    | Board x -> List.fold_left (fun h x ->
                     get_entry_height h x.child) height x.ports

let get_node_height t =
  let height = 0 in
  match t with
  | `CPU x    -> List.fold_left (fun h x -> get_entry_height h x.conn) height x.ifaces
  | `Boards x -> List.fold_left(fun h x ->
                       List.fold_left (fun h1 x1 ->
                           get_entry_height h1 x1.child) h x.ports) height x
let get_list_height l =
  List.fold_left (fun height x -> height + get_entry_height 0 x) 0 l

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

(*calculates the max depth in the tree*)
let get_list_depth l =
  List.fold_left (fun depth x -> max (get_entry_depth 0 x) depth ) 0 l

type line = | Up | Down | Str

type color = | Black | White | Green | Red | Grey

let color_to_string = function
  | Black -> "RGB(0,0,0)"
  | White -> "RGB(255,255,255)"
  | Green -> "RGB(77,177,54)"
  | Red   -> "RGB(246,47,54)"
  | Grey  -> "RGB(159,159,159)"

let draw_line ~(color : color) ~(width : int) ~(height : int) ~(typ : line) =
  let left_half  = width / 6 in
  let center     = width / 2 in
  let right_half = width * 5 / 6 in
  let bottom     = height * 2 / 3 in
  let line =
    match typ with
    | Up    -> Printf.sprintf "M 0 %d L %d %d Q %d %d, %d %d L %d %d Q %d 1, %d 1 L %d 1"
                 (height - 1)
                 left_half (height - 1)
                 center (height - 1)
                 center bottom
                 center (height / 3)
                 center
                 right_half
                 width

    | Down  -> Printf.sprintf "M 0 1 L %d 1 Q %d 1, %d %d L %d %d Q %d %d, %d %d L %d %d"
                 left_half
                 center
                 center (height / 3)
                 center bottom
                 center (height - 1)
                 right_half (height - 1)
                 width (height - 1)

    | Str   -> Printf.sprintf "M 0 %d
                               L %d %d"
                 (height/2)
                 width (height/2)
  in
  Html.svg ~a:([Svg.a_height @@ (float_of_int height, Some `Px)
               ; Svg.a_width  @@ (float_of_int width, Some `Px)])
    [ Svg.path ~a:([ Svg.a_fill `None
                   ; Svg.a_stroke_width (2., None)
                   ; Svg.a_stroke (`Color ((color_to_string color),None))
                   ; Svg.a_d line]) []]
  |> Tyxml_js.To_dom.of_element

let rm_children container =
  Dom.list_of_nodeList @@ container##.childNodes
  |> List.iter (fun x -> Dom.removeChild container x)

let topo_boards =
  let rec f acc = (function
                   | Board b -> List.fold_left (fun a x -> f a x.child) (b :: acc) b.ports
                   | Input _ -> acc) in
  List.fold_left f []

let concat (l : string list) : string =
  List.fold_left (fun acc x -> x^" "^acc) "" l

let grid_template_areas t =
  let depth = get_node_depth t in
  let rec get_entry_areas acc count = function
    | Input x -> if (count+1) < depth
                 then let inp  = "\""^(input_to_area x) in
                      let list = CCList.range 1 (depth-count-1) in
                      (List.fold_left (fun acc _ -> acc^" . ") inp list)^acc^"\""
                 else "\""^(input_to_area x)^" "^acc^"\""
    | Board x -> (let ports = List.map (fun x -> x.child) x.ports in
                  let str   = (board_to_area x)^" " in
                  match ports with
                  | [] -> str^" "^acc
                  | l  -> concat (List.map (get_entry_areas (str^acc) (count+1)) l))
  in
  match t with
  | `CPU x    -> concat (List.map (fun x -> get_entry_areas "CPU" 1 x.conn) x.ifaces)
  | `Boards x -> concat
                   (List.map (fun board ->
                        concat (List.map (fun x ->
                                    get_entry_areas (board_to_area board) 1 x.child)
                                  board.ports)
                      ) x)

let render ?on_click ~topology ~(width : int) ~topo_el () =
  let gta = "grid-template-areas: "^(grid_template_areas topology)^";" in
(*  let gta = "grid-template-areas: \"a b c d e\" \". f c d e\" \"g h i d e\" \". k i d e\";" in*)
  print_endline gta;
  let depth, height = get_node_depth topology, get_node_height topology in
  let boards = topo_boards @@ Common.Topology.get_entries topology in
  rm_children topo_el;
  let _ = List.iter (fun x ->
              let b = Topo_board.create x in
              b#style##.minWidth := Js.string @@ (string_of_int (width/2/depth))^"px";
              Dom.appendChild topo_el b#root;
              let css_text area = b#style##.cssText := Js.string area in
              css_text ("grid-area: "^(board_to_area x)^";");)boards
  in
  topo_el##.style##.cssText := Js.string gta;
  let cell_width  = width / (depth + 1) in
  let conn_width  = cell_width * 3 / 10 in
  let item_width  = cell_width - conn_width in
  let item_height = item_width / 4 * 3 in
  let div_height  = item_height * height + 1 in
  topo_el##.style##.display   := Js.string "grid";
  topo_el##.style##.marginTop := Js.string "64px";
(*  topo_el##.style##.width     := Js.string @@ (string_of_int width)^"px";
  topo_el##.style##.height    := Js.string @@ (string_of_int div_height)^"px";*)
  let s1 = draw_line ~color:Green ~width:0 ~height:0 ~typ:Str in
  s1##.classList##add (Js.string "line");
  let list   = topo_el##querySelectorAll (Js.string "line") in
  let length = list##.length in
  for i=0 to length-1
  do
    (try Js.Opt.iter (list##item i) (fun x -> Dom.removeChild topo_el x) with _ -> ())
  done;
  Printf.printf "Depth is %d and height is %d\n" depth height;
  Printf.printf "Item w h %d %d\n" item_width item_height
