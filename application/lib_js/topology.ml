[@@@ocaml.warning "-60"]

open Js_of_ocaml
open Common.Topology
open Containers
open Tyxml_js
open Components

let px x = Js.string @@ (string_of_int x)^"px"

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

let draw_line ~(color : color) ~(width : int) ~(height : int) ~(typ : line) ~left ~top =
  let left_half  = width / 6 in
  let center     = width / 2 in
  let right_half = width * 5 / 6 in
  let bottom     = height * 2 / 3 in
  let path       =
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
  let line = Html.svg ~a:([Svg.a_height @@ (float_of_int height, Some `Px)
                          ; Svg.a_width  @@ (float_of_int width, Some `Px)])
               [ Svg.path ~a:([ Svg.a_fill `None
                              ; Svg.a_stroke_width (2., None)
                              ; Svg.a_stroke (`Color ((color_to_string color),None))
                              ; Svg.a_d path]) []]
             |> Tyxml_js.To_dom.of_element
  in
  line##.style##.left := px left;
  line##.style##.top  := px top;
  line##.style##.position := Js.string "absolute";
  line

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
  | `Boards x -> concat (List.map (fun board ->
                             concat (List.map (fun x ->
                                         get_entry_areas (board_to_area board) 1 x.child)
                                       board.ports)) x)

let connect_elements ~parent ~start_el ~end_el ~color ~x_level =
  let start_el, end_el = if start_el##.offsetLeft < end_el##.offsetLeft
                         then start_el, end_el
                         else end_el, start_el
  in
  let start_x = start_el##.offsetLeft + start_el##.offsetWidth in
  let start_y = start_el##.offsetTop + start_el##.offsetHeight / 2 in
  let end_x   = end_el##.offsetLeft in
  let end_y   = end_el##.offsetTop + end_el##.offsetHeight / 2 in
  let width   = end_x - start_x in
  if width / x_level <= 1
  then
    let top, height, typ = if end_y = start_y
                           then start_y, 2, Str
                           else
                             if  end_y <= start_y
                             then start_y, start_y - end_y, Up
                             else end_y, end_y - start_y, Down
    in
    Dom.appendChild parent @@
      draw_line ~color ~width ~height ~typ ~left:start_x ~top
  else
    if end_y = start_y
    then
      Dom.appendChild parent @@
        draw_line ~color ~width ~height:2 ~typ:Str ~left:start_x ~top:start_y
    else
      if  end_y <= start_y
      then
        (Dom.appendChild parent @@
           draw_line ~color ~width:(width-x_level) ~height:2 ~typ:Str ~left:start_x ~top:start_y;
         Dom.appendChild parent @@
           draw_line ~color ~width:x_level ~height:(start_y - end_y)
             ~typ:Up ~left:(end_x-x_level) ~top:start_y)
      else (Dom.appendChild parent @@
              draw_line ~color ~width:(width-x_level) ~height:2 ~typ:Str ~left:start_x ~top:start_y;
            Dom.appendChild parent @@
              draw_line ~color ~width:x_level ~height:(end_y-start_y)
                ~typ:Down ~left:(end_x-x_level) ~top:end_y)


let draw_topology ~topo_el ~topology =
  let create_board board =
    let brd = Topo_board.create board in
    let div = Dom_html.createDiv Dom_html.document in
    div##.style##.cssText :=
      Js.string @@ "grid-area: "^(board_to_area board)^";";
    div##.style##.margin := Js.string "auto 0";
    Dom.appendChild div brd#root;
    Dom.appendChild topo_el div;
    brd
  in
  let rec get_boards acc = function
    | Input _ -> ()
    | Board x -> (let ports = List.map (fun x -> x.child) x.ports in
                  let b = create_board x in
                  match ports with
                  | [] -> ()
                  | l  -> List.iter (get_boards (b::acc)) l)
  in
  match topology with
  | `CPU x    -> List.iter (fun x -> get_boards [] x.conn) x.ifaces
  | `Boards x -> List.iter (fun board -> let b = create_board board in
                                         List.iter (fun x -> get_boards [b] x.child) board.ports
                   ) x

let render ?on_click ~topology ~(width : int) ~topo_el () =
  let gta = "grid-auto-rows: 1fr; grid-auto-columns: 1fr; grid-template-areas: "^(grid_template_areas topology)^";" in
(*  let gta = "grid-template-areas: \"a b c d e\" \". f c d e\" \"g h i d e\" \". k i d e\";" in*)
  print_endline gta;
  topo_el##.style##.cssText   := Js.string gta;
  topo_el##.style##.display   := Js.string "grid";
  topo_el##.style##.marginTop := Js.string "64px";
  rm_children topo_el;
  draw_topology ~topo_el ~topology
