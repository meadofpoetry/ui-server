[@@@ocaml.warning "-60"]

open Js_of_ocaml
open Gg
open Vg
open Common.Topology

let text ~text pos f = I.cut_glyphs f ~text [] (I.const Color.black)
                       >> I.scale (V2.v 1. 0.8)
                       >> I.move pos

let input_to_string ({ input; id }:topo_input) =
  (match input with
   | RF    -> "RF "
   | TSOIP -> "TSoIP "
   | ASI   -> "ASI ") ^ (string_of_int id)

let board_to_string (tp:typ) =
  match tp with
  | DVB   -> "Adapter DVB"
  | TS    -> "Adapter TS"
  | IP2TS -> "Adapter IP"
  | TS2IP -> "Converter IP"

let floor_to_five x =
  let fx = floor x +. 0.5 in
  if (x > fx) then fx else floor x

let ceil_to_five x =
  let cx = ceil x -. 0.5 in
  if (x < cx) then cx else ceil x

let find_max l =
  let rec f = (fun max l -> (match l with
                             | [] -> max
                             | hd :: tl -> f (if hd > max then hd else max) tl)) in
  (match l with
   | [] -> failwith "find max: list is empty"
   | hd :: tl -> f hd tl)

let rec get_node_height height = function
  | Input _ -> height + 1
  | Board x -> List.fold_left (fun h x -> get_node_height h x.child ) height x.ports

let get_list_height l =
  List.fold_left (fun height x -> height + 1 + get_node_height 0 x) 0 l

let rec get_node_depth depth = function
  | Input _ -> succ depth
  | Board x -> (let ports = List.map (fun x -> x.child) x.ports in
                match ports with
                | [] -> succ depth
                | l  -> succ @@ find_max (List.map (get_node_depth depth) l))

let get_list_depth l =
  List.fold_left (fun depth x -> max (get_node_depth 0 x) depth ) 0 l

let rec setting t x y y1 height l =
    match t with
    | Board el ->
      let l = CCList.Assoc.set (x , y) (Board el) l in
      let l = CCList.Assoc.set (x +. 0.5,y) (Board el) l in
      let l =
        if (y +. 0.5) < y1 +. height
        then setting t x (y +. 0.5) y1 height l
        else l
      in
      l
    | Input el ->
      let l = CCList.Assoc.set (x +. 0.5,y) (Input el) l in
      let l =
        if (y +. 0.5) < y1 +. height
        then setting t x (y +. 0.5) y1 height l
        else l
      in
      l

let converter_ports y port number =
     match (port, number) with
      | (0,1) -> y
      | (0,2) -> y +. 2.
      | (1,2) -> y
      | (0,3) -> y +. 4.
      | (1,3) -> y +. 2.
      | (2,3) -> y
      | (0,4) -> y +. 6.
      | (1,4) -> y +. 4.
      | (2,4) -> y +. 2.
      | (3,4) -> y
      | (0,5) -> y +. 8.
      | (1,5) -> y +. 6.
      | (2,5) -> y +. 4.
      | (3,5) -> y +. 2.
      | (4,5) -> y
      | (_,_) -> 0.

let green = I.const (Color.v 0.1 1.0 0.1 0.95)
let red   = I.const (Color.v 1.0 0.1 0.1 0.95)

let size x y= Size2.v (float_of_int x *. 0.26458333) (float_of_int y *. 0.26458333)

let render canvas size image =
  let r = Vgr.create (Vgr_htmlc.target canvas) `Other in
  ignore (Vgr.render r (`Image (size, Box2.unit, image)));
  ignore (Vgr.render r `End)

let prompt quest def= Js.Opt.get (Dom_html.window##prompt (Js.string quest) (Js.string def))
                                 (fun () -> Js.string def)
                      |> Js.to_string

let true_size x_draw y_draw x y cols raws = P2.v ((x_draw +. x) /. cols) ((y_draw +. y) /.raws)

module Port = struct

  let draw_qoe acc x1 y cols rows =
    let sz x_draw y_draw = true_size x_draw y_draw (x1 +. 2.) y cols rows in
    let p = P.empty
            >> P.sub    (sz 0.0 0.2)
            >> P.line   (sz 0.0 0.8)
            >> P.ccurve (sz 0.0 0.8) (sz 0.0 1.0) (sz 0.2 1.0)
            >> P.line   (sz 0.8 1.0)
            >> P.ccurve (sz 0.8 1.0) (sz 1.0 1.0) (sz 1.0 0.8)
            >> P.line   (sz 1.0 0.2)
            >> P.ccurve (sz 1.0 0.2) (sz 1.0 0.0) (sz 0.8 0.0)
            >> P.line   (sz 0.2 0.0)
            >> P.ccurve (sz 0.2 0.0) (sz 0.0 0.0) (sz 0.0 0.2) in
    let img = I.const @@ Color.v 0.1 0.1 0.1 1.0 >> I.cut p in
    (I.blend img acc)

  let draw_input t (acc, acc_top) x y cols rows =
    let font    = Font.{ name = ""; slant = `Normal; weight = `W400; size = 0.012 } in
    let acc_top = setting (Input t) x y y 1. acc_top in
    let area = `O { P.o with P.width = 0.0008 } in
    let sz x_draw y_draw = true_size x_draw y_draw x y cols rows in
    let p = P.empty (*a box*)
            >> P.sub    (sz 0.45 0.50)
            >> P.line   (sz 0.45 0.60)
            >> P.ccurve (sz 0.45 0.60) (sz 0.45 0.70) (sz 0.55 0.70)
            >> P.line   (sz 0.90 0.70)
            >> P.ccurve (sz 0.90 0.70) (sz 1.00 0.70) (sz 1.00 0.60)
            >> P.line   (sz 1.00 0.40)
            >> P.ccurve (sz 1.00 0.40) (sz 1.00 0.30) (sz 0.90 0.30)
            >> P.line   (sz 0.55 0.30)
            >> P.ccurve (sz 0.55 0.30) (sz 0.45 0.30) (sz 0.45 0.40)
            >> P.line   (sz 0.45 0.50) in
    let input = I.const Color.black >> I.cut ~area p in
    let with_label = I.blend (text ~text:(input_to_string t)
                                   (true_size 0.48 0.0 x (y+.0.44) cols rows)
                                   {font with size = (0.12/.cols); weight = `W100})
                             input in
    ((I.blend with_label acc),acc_top)

  let draw_straight x1 y1 x2 _ cols rows =
    let x_delta = x2 -. x1 in
    P.empty
    >> P.sub  (true_size 0.0     0.0 x1 y1 cols rows)
    >> P.line (true_size x_delta 0.0 x1 y1 cols rows)

  let draw_long_if_parent_higher x1 y1 x2 y2 cols rows =
    let x_delta = x2 -. x1 in
    let y_delta = y2 -. y1 in
    let sz x_draw y_draw x = true_size x_draw y_draw x y1 cols rows in
    P.empty
    >> P.sub    (sz 0.0 0.0 x1)
    >> P.line   (sz (x_delta -. 1.0) 0.0 x1 ) (* we draw the straight line to input for x_delta-1 *)
    >> P.sub    (sz 0.0 0.0 (x2 -. 1.0))
    >> P.line   (sz 0.4 0.0 (x2 -. 1.0))
    >> P.ccurve (sz 0.4 0.0 (x2 -. 1.0))  (*and then a curve*)
                (sz 0.5 0.0 (x2 -. 1.0))
                (sz 0.5 (0.1 *. y_delta) (x2 -. 1.0))
    >> P.line   (sz 0.5 (0.9 *. y_delta) (x2 -. 1.0))
    >> P.ccurve (sz 0.5 (0.9 *. y_delta) (x2 -. 1.0))
                (sz 0.5 y_delta (x2 -. 1.0))
                (sz 0.6 y_delta (x2 -. 1.0))
    >> P.line   (sz 1.0 y_delta (x2 -. 1.0))

  let draw_long_if_child_higher x1 y1 x2 y2 cols rows =
    let x_delta = x2 -. x1 in
    let y_delta = y1 -. y2 in
    let sz x_draw y_draw x y = true_size x_draw y_draw x y cols rows in
    P.empty
    >> P.sub    (sz 0.0 0.0 x1 y1) (*a straight line to input of length x_delta-1*)
    >> P.line   (sz (x_delta -. 1.0) 0.0 x1 y1)
    >> P.sub    (sz 0.0 y_delta (x2 -. 1.0) y2) (*and then a curve*)
    >> P.line   (sz 0.4 y_delta (x2 -. 1.0) y2)
    >> P.ccurve (sz 0.4 y_delta (x2 -. 1.0) y2)
                (sz 0.5 y_delta (x2 -. 1.0) y2)
                (sz 0.5 (0.9 *. y_delta) (x2 -. 1.0) y2)
    >> P.line   (sz 0.5 (0.1 *.y_delta) (x2 -. 1.0) y2)
    >> P.ccurve (sz 0.5 (0.1 *.y_delta) (x2 -. 1.0) y2)
                (sz 0.5 0.0 (x2 -. 1.0) y2)
                (sz 0.6 0.0 (x2 -. 1.0) y2)
    >> P.line   (sz 1.0 0.0 (x2 -. 1.0) y2)

  let draw_short_if_parent_higher x1 y1 x2 y2 cols rows =
    let x_delta = x2 -. x1 in
    let y_delta = y2 -. y1 in
    let sz x_draw y_draw = true_size x_draw y_draw x1 y1 cols rows in
    P.empty
    >> P.sub    (sz 0.0 0.0)
    >> P.line   (sz (0.4 *. x_delta) 0.0)
    >> P.ccurve (sz (0.4 *. x_delta) 0.0)
                (sz (0.5 *. x_delta) 0.0)
                (sz (0.5 *. x_delta) (0.1 *. y_delta))
    >> P.line   (sz (0.5 *. x_delta) (0.9 *. y_delta))
    >> P.ccurve (sz (0.5 *. x_delta) (0.9 *. y_delta))
                (sz (0.5 *. x_delta) y_delta)
                (sz (0.6 *. x_delta) y_delta)
    >> P.line   (sz x_delta y_delta)

  let draw_short_if_child_higher x1 y1 x2 y2 cols rows =
    let x_delta = x2 -. x1 in
    let y_delta = y1 -. y2 in
    let sz x_draw y_draw = true_size x_draw y_draw x1 y2 cols rows in
    P.empty
    >> P.sub    (sz 0.0 y_delta)
    >> P.line   (sz (0.4 *. x_delta) y_delta)
    >> P.ccurve (sz (0.4 *. x_delta) y_delta)
                (sz (0.5 *. x_delta) y_delta)
                (sz (0.5 *. x_delta) (0.9 *. y_delta))
    >> P.line   (sz (0.5 *. x_delta) (0.1 *. y_delta))
    >> P.ccurve (sz (0.5 *. x_delta) (0.1 *. y_delta))
                (sz (0.5 *. x_delta) 0.0)
                (sz (0.6 *. x_delta) 0.0)
    >> P.line   (sz x_delta 0.0)

  (* draws a line between elements of given x and y. usually x1,y1 is a child and x2,y2 is a parent *)
  let draw_line acc colour x1 y_1 x2 y2 cols rows =
    let y1      = y_1 +. 0.5 in
    let area    = `O { P.o with P.width = 1. /. rows /. 50. } in
    (if y1 = y2
     then draw_straight x1 y1 x2 y2 cols rows
     else if (x2 -. x1) >= 2.0
     then (if y2 > y1
           then draw_long_if_parent_higher x1 y1 x2 y2 cols rows
           else draw_long_if_child_higher x1 y1 x2 y2 cols rows)
     else if y2 > y1
     then draw_short_if_parent_higher x1 y1 x2 y2 cols rows
     else draw_short_if_child_higher x1 y1 x2 y2 cols rows)
    |> (fun p -> let img = colour >> I.cut ~area p in
                 I.blend img acc)

end

module Board = struct

  let rec draw t (acc, acc_top) x y cols rows =
    Printf.printf "x %f y %f cols %f rows %f\n" x y cols rows;
    let font       = Font.{ name = ""; slant = `Normal; weight = `W400; size = 0.012 } in
    let num        = List.length t.ports in
    (* the more children converter has the bigger it is *)
    let height     = if t.typ = TS2IP then (float_of_int num *. 2.) else 1. in
    (*then we must place converter in a right place due to its height*)
    let y          = if t.typ = TS2IP then y -. 0.5 else y in
    let acc_top    = setting (Board t) x y y height acc_top in
    let area       = `O {P.o with P.width = 0.0008 } in
    let board_form = (* a path to draw a board itself *)
      let sz x_draw y_draw = true_size x_draw y_draw x y cols rows in
      P.empty
      >> P.sub    (sz 0.00  0.15)
      >> P.line   (sz 0.00 (height -. 0.15))
      >> P.ccurve (sz 0.00 (height -. 0.15))
                  (sz 0.00 (height -. 0.1))
                  (sz 0.05 (height -. 0.1))
      >> P.line   (sz 0.95 (height -. 0.1))
      >> P.ccurve (sz 0.95 (height -. 0.1))
                  (sz 1.00 (height -. 0.1))
                  (sz 1.00 (height -. 0.15))
      >> P.line   (sz 1.00 0.15)
      >> P.ccurve (sz 1.00 0.15) (sz 1.00 0.10) (sz 0.95 0.10)
      >> P.line   (sz 0.05  0.10)
      >> P.ccurve (sz 0.05 0.10) (sz 0.00 0.10) (sz 0.00 0.15)
    in
    let shadow_form = (* a path to draw its shadow *)
      let sz x_draw y_draw x y = true_size x_draw y_draw x y cols rows in
      P.empty
      >> P.sub    (sz 0.00  0.15            (x-.0.02) (y-.0.03))
      >> P.line   (sz 0.00 (height -. 0.15) (x-.0.02) (y+.0.02))
      >> P.ccurve (sz 0.00 (height -. 0.15) (x-.0.02) (y+.0.02))
                  (sz 0.00 (height -. 0.10) (x-.0.02) (y+.0.02))
                  (sz 0.05 (height -. 0.10) (x-.0.02) (y+.0.02))
      >> P.line   (sz 0.95 (height -. 0.10) (x+.0.03) (y+.0.02))
      >> P.ccurve (sz 0.95 (height -. 0.10) (x+.0.03) (y+.0.02))
                  (sz 1.00 (height -. 0.10) (x+.0.03) (y+.0.02))
                  (sz 1.00 (height -. 0.15) (x+.0.03) (y+.0.02))
      >> P.line   (sz 1.00 0.15 (x+.0.03) (y-.0.03))
      >> P.ccurve (sz 1.00 0.15 (x+.0.03) (y-.0.03))
                  (sz 1.00 0.10 (x+.0.03) (y-.0.03))
                  (sz 0.95 0.10 (x+.0.03) (y-.0.03))
      >> P.line   (sz 0.05 0.10 (x-.0.02) (y-.0.03))
      >> P.ccurve (sz 0.05 0.10 (x-.0.02) (y-.0.03))
                  (sz 0.00 0.10 (x-.0.02) (y-.0.03))
                  (sz 0.00 0.15 (x-.0.02) (y-.0.03))
    in
    (* a color of a board depends on its state, a color of border is the same but brighter *)
    let colour, border_col =
      match t.connection with
      | `Fine        -> Color.v 0.7 1.0 0.7 1.0, Color.v 0.1 1.0 0.1 1.0
      | `No_response -> Color.v 1.0 0.7 0.7 1.0, Color.v 1.0 0.1 0.1 1.0
      | `Init        -> Color.v 0.8 0.8 0.8 1.0, Color.v 0.2 0.2 0.2 1.0
    in
    let border      = I.const border_col >> I.cut ~area board_form in  (*draw border*)
    let board       = I.const colour  >> I.cut board_form in           (*draw board*)
    let shadow      = I.const @@ Color.v 0.8 0.8 0.8 0.6 >> I.cut shadow_form in
    let with_shadow = I.blend board shadow in         (*then we blend images together*)
    let with_border = I.blend border with_shadow in
    let with_label  = I.blend (text ~text:(board_to_string t.typ)
                                    (true_size 0.1 0.05 x (y +. height -. 1. +. 0.44) cols rows)
                                    { font with size = (0.13 /. cols); weight = `W400 })
                              with_border in
    let with_model  = I.blend (text ~text:t.model
                                    (true_size 0.1 0.05 x (y +. height -. 1. +. 0.27) cols rows)
                                    { font with size = (0.08 /. cols); weight = `W100 })
                              with_label in
    let with_man    = I.blend (text ~text:t.manufacturer
                                    (true_size 0.1 0.05 x (y +. height -.1. +. 0.15) cols rows)
                                    { font with size = (0.08/.cols); weight = `W100 })
                              with_model in
    let img1        = I.blend with_man acc in
    match t.ports with
    | [] -> (img1,acc_top)
    | l  ->
       CCList.foldi (fun (acc,acc_top) i z ->
           let colour1 = if z.listening then green else red in
           match t.typ with
           | TS2IP -> let y_child = converter_ports y i num in
                      let draw_line x1 = (Port.draw_line acc colour1 x1 y_child x (y_child +. 0.5) cols rows),
                                         acc_top in
                      (match z.child with
                       | Input el -> Port.draw_input el (draw_line 2.0) 1.0 y_child cols rows
                       | Board el -> draw el (draw_line @@ x -. 1.0) (x -. 2.0) y_child cols rows)
           | _ -> let y_child = (match i,num with
                                 | (0,1) | (1,3) -> y
                                 | (0,2)         -> y +. 0.5
                                 | (1,2)         -> y -. 0.5
                                 | (0,3)         -> y +. 1.
                                 | (2,3)         -> y -. 1.
                                 | (_,_)         -> 0.) in
                  let y2 = y +. 1.0 -. (float_of_int (i + 1)) /. ((float_of_int num) +. 1.0) in
                  (match z.child with
                   | Input el -> let line = (Port.draw_line acc colour1 2.0 y_child x y2 cols rows), acc_top in
                                 Port.draw_input el line 1.0 y_child cols rows
                   | Board el -> let line = (Port.draw_line acc colour1 (x -. 1.0) y_child x y2 cols rows),
                                            acc_top in
                                 draw el line (x -. 2.0) y_child cols rows))
                    (img1,acc_top) l
end

module Entry = struct

  let draw t (acc, acc_top, num) cols rows h =
    let cols_f = float_of_int cols in
    let rows_f = float_of_int rows in
    let (acc1,acc_top1) =
      match t with
      | Input x -> Port.draw_input x (acc, acc_top) 1.0 (float_of_int @@ num + h) cols_f rows_f
      | Board x -> Board.draw x (acc, acc_top) (float_of_int cols -. 2.0) (float_of_int num) cols_f rows_f
    in
    (acc1, acc_top1, num + h)

end

let render ?on_click ~topology ~(width : int) ~canvas () =
  let height = ((Js.Optdef.to_option @@ Dom_html.window##.innerHeight
                 |> CCOpt.get_or ~default:0) -16) / 10 in
  let t      = topology in
  let cols   = (get_list_depth t * 2 + 1) in
  let rows   = (get_list_height t + 1) in
  let ar     = 4. /. 4. in
  let cw     = width / cols in
  let rh     = int_of_float @@ (float_of_int cw) /. ar in
  let start  = I.const @@ Color.v 1.0 1.0 1.0 0.0
               >> I.cut (P.empty
                         >> P.sub  (P2.v 0. 0.)
                         >> P.line (P2.v 0. 1.)
                         >> P.line (P2.v 1. 1.)
                         >> P.line (P2.v 1. 0.)
                         >> P.line (P2.v 0. 0.)) in
  let draw_entry = (fun (acc_img,acc_top,number) x ->
      Entry.draw x (acc_img, acc_top, number) cols rows (get_node_height 0 x + 1)) in
  let (acc_img, acc_top, _) = List.fold_left draw_entry (start,[],1) t in
  render canvas (size (cw * cols) (rh * rows)) acc_img;
  match on_click with
  | Some f -> (fun e -> let width = width / cols in
                        let x,y = (Js.Optdef.to_option @@ e##.pageX
                                   |> CCOpt.get_or ~default:0) - 8,
                                  (Js.Optdef.to_option @@ e##.pageY
                                   |> CCOpt.get_or ~default:0) - 8 in (* margin *)
                        let x1,y1 = floor_to_five (float_of_int x /. float_of_int width),
                                    floor_to_five (float_of_int rows -. float_of_int y /. float_of_int height) in
                        (match CCList.Assoc.get (x1,y1) acc_top with
                         | None       -> ()
                         | Some entry -> f entry);
                        Js._false)
              |> (fun x -> canvas##.onmousedown := Dom_html.handler x)
  | None -> ()
