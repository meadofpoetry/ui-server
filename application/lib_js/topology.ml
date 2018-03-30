[@@@ocaml.warning "-60"]

open Js_of_ocaml
open Common.Topology
open Containers
open Tyxml_js

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
  Html.svg ~a:([ Svg.a_height @@ (float_of_int height, Some `Px)
               ; Svg.a_width  @@ (float_of_int width, Some `Px)])
    [ Svg.path ~a:([ Svg.a_fill `None
                   ; Svg.a_stroke_width (2., None)
                   ; Svg.a_stroke (`Color ((color_to_string color),None))
                   ; Svg.a_d line]) []]
  |> Tyxml_js.To_dom.of_element


let render ?on_click ~topology ~(width : int) ~topo_el () =
  let depth, height = get_node_depth topology, get_node_height topology in
  let cell_width    = width / (depth + 1) in
  let conn_width    = cell_width * 3 / 10 in
  let item_width    = cell_width - conn_width in
  let item_height   = item_width / 4 * 3 in
  let div_height    = item_height * height * 2 + 1 in
  topo_el##.style##.marginTop := Js.string "64px";
  topo_el##.style##.width     := Js.string @@ (string_of_int width)^"px";
  topo_el##.style##.height    := Js.string @@ (string_of_int div_height)^"px";
  let up, str, down = draw_line ~color:Red   ~width:100 ~height:100 ~typ:Up,
                      draw_line ~color:Green ~width:100 ~height:100 ~typ:Str,
                      draw_line ~color:Grey  ~width:100 ~height:100 ~typ:Down
  in
  up##.id   := Js.string "1";
  down##.id := Js.string "2";
  str##.id  := Js.string "3";
  (try Dom.removeChild topo_el (Dom_html.getElementById "1") with _ -> ());
  (try Dom.removeChild topo_el (Dom_html.getElementById "2") with _ -> ());
  (try Dom.removeChild topo_el (Dom_html.getElementById "3") with _ -> ());
  Dom.appendChild topo_el up;
  Dom.appendChild topo_el str;
  Dom.appendChild topo_el down;
  Printf.printf "Depth is %d and height is %d\n" depth height;
  Printf.printf "Item w h %d %d\n" item_width item_height
