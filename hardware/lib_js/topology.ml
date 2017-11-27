[@@@ocaml.warning "-60"]

open Js_of_ocaml
open Gg
open Vg
open Common.Topology

(*All positioning constants in GG and VG mean: *)
(*X from left to right and Y from bottom to top*)
(*    0.0,1.0 __________________  1.0,1.0      *)
(*           |                  |              *)
(*           |                  |              *)
(*           |                  |              *)
(*    0.0,0.0|__________________| 1.0,0.0      *)

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
  | TS2IP -> "ConverterIP"

let floor_to_five x =
  let fx = floor x +. 0.5 in
  if (x > fx) then fx else floor x

let ceil_to_five x =
  let cx = ceil x -. 0.5 in
  if (x < cx) then cx else ceil x

let find_max l =
  let rec f = (fun max l ->
      (match l with
       | [] -> max
       | hd :: tl -> f (if hd > max then hd else max) tl)) in
  (match l with
   | [] -> failwith "find max: list is empty"
   | hd :: tl -> f hd tl)

(*calculates the height of a node*)
let rec get_node_height height = function
  | Input _ -> height + 1
  | Board x -> List.fold_left (fun h x ->
                   get_node_height h x.child) height x.ports

(*calculates the height of the whole list*)
let get_list_height l =
  List.fold_left (fun height x -> height + get_node_height 0 x) 0 l

(*calculates the depth of the node*)
let rec get_node_depth depth = function
  | Input _ -> succ depth
  | Board x -> (let ports = List.map (fun x -> x.child) x.ports in
                match ports with
                | [] -> succ depth
                | l  -> succ @@ find_max (List.map (get_node_depth depth) l))

(*calculates the max depth in the tree*)
let get_list_depth l =
  List.fold_left (fun depth x -> max (get_node_depth 0 x) depth ) 0 l

(*a function for setting associations in a list according to position and board/input*)
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
      let l = CCList.Assoc.set (x,y) (Input el) l in
      let l =
        if (y +. 0.5) < y1 +. height
        then setting t x (y +. 0.5) y1 height l
        else l
      in
      l

let green = I.const (Color.v 0.1 1.0 0.1 0.95)   (*a green image, slightly transparent*)
let red   = I.const (Color.v 1.0 0.1 0.1 0.95)   (*a red image*)
let gray  = I.const (Color.v 0.4 0.4 0.4 0.95)   (*a gray image*)

(*px to mm*)
let size x y = Size2.v (float_of_int x *. 0.26458333) (float_of_int y *. 0.26458333)

(*a function for rendering an image to an html canvas of given size*)
let render canvas size image =
  let r = Vgr.create (Vgr_htmlc.target canvas) `Other in
  ignore (Vgr.render r (`Image (size, Box2.unit, image)));
  ignore (Vgr.render r `End)

(*just a function for prompt. is not needed at the moment actually, but still might be useful*)
let prompt quest def= Js.Opt.get (Dom_html.window##prompt (Js.string quest) (Js.string def))
                                 (fun () -> Js.string def)
                      |> Js.to_string
(*returns the size considering x and y, rows and cols (size for vg in 1./.x etc.)*)
let true_size x_draw y_draw x y cols raws = P2.v ((x_draw +. x) /. cols) ((y_draw +. y) /.raws)

module Port = struct
  (*a function for drawing qoe box. isn't used yet, but might be*)
  let draw_qoe acc x1 y cols rows =
    let sz x_draw y_draw = true_size x_draw y_draw (x1 +. 2.) y cols rows in
    let p = P.empty                                                    (*an element          - a part of it*)
            >> P.sub    (sz 0.0 0.2)                                   (*left bottom corner  - top left*)
            >> P.line   (sz 0.0 0.8)                                   (*left top corner     - bottom left*)
            >> P.ccurve (sz 0.0 0.8) (sz 0.0 1.0) (sz 0.2 1.0)         (*left top corner     - a curve itself*)
            >> P.line   (sz 0.8 1.0)                                   (*right top corner    - top left*)
            >> P.ccurve (sz 0.8 1.0) (sz 1.0 1.0) (sz 1.0 0.8)         (*right top corner    - a curve itself*)
            >> P.line   (sz 1.0 0.2)                                   (*right bottom corner - top right*)
            >> P.ccurve (sz 1.0 0.2) (sz 1.0 0.0) (sz 0.8 0.0)         (*right bottom corner - a curve itself*)
            >> P.line   (sz 0.2 0.0)                                   (*left bottom corner  - bottom right*)
            >> P.ccurve (sz 0.2 0.0) (sz 0.0 0.0) (sz 0.0 0.2) in      (*left bottom corner  - a curve itself*)
    let img = I.const @@ Color.v 0.1 0.1 0.1 1.0 >> I.cut p in         (*cut an image of color gray with path p*)
    (I.blend img acc)                                                  (*blend image with acc *)

  let draw_input t (acc, acc_top) x y cols rows =                      (*drawing an input element*)
    let font    = Font.{ name = ""; slant = `Normal; weight = `W400; size = 0.012 } in
    let acc_top = setting (Input t) x y y 1. acc_top in                (*setting a name of input to the list*)
    let area = `O { P.o with P.width = 0.04 /. cols /. rows } in       (*a void area with given width of border*)
    let sz x_draw y_draw = true_size x_draw y_draw x y cols rows in    (*a size and a position of an element*)
    let p = P.empty                                                    (*a box*)
            >> P.sub    (sz 0.01 0.50)                                 (*center left*)
            >> P.line   (sz 0.01 0.60)                                 (*left top corner     - left bottom*)
            >> P.ccurve (sz 0.01 0.62) (sz 0.01 0.70) (sz 0.10 0.70)   (*left top corner     - a curve itself*)
            >> P.line   (sz 0.40 0.70)                                 (*right top corner    - left top*)
            >> P.ccurve (sz 0.42 0.70) (sz 0.50 0.70) (sz 0.50 0.60)   (*right top corner    - a curve itself*)
            >> P.line   (sz 0.50 0.40)                                 (*right bottom corner - right top*)
            >> P.ccurve (sz 0.50 0.38) (sz 0.50 0.30) (sz 0.40 0.30)   (*right bottom corner - a curve itself*)
            >> P.line   (sz 0.10 0.30)                                 (*left bottom corner  - right bottom*)
            >> P.ccurve (sz 0.08 0.30) (sz 0.01 0.30) (sz 0.01 0.40)   (*left bottom corner  - a curve itself*)
            >> P.line   (sz 0.01 0.50) in                              (*center left*)
    let input = I.const Color.black >> I.cut ~area p in                (*cut input void black image with path p*)
    let with_label = I.blend (text ~text:(input_to_string t)           (*blend it with input's label*)
                                (true_size 0.05 0.0 x (y+.0.46) cols rows)
                                {font with size = (0.12/.cols); weight = `W100})
                       input in
    ((I.blend with_label acc),acc_top)                                 (*blend it with acc*)

  let draw_straight x1 y1 x2 _ cols rows =                             (*draws a straight line from element to element*)
    let x_delta = x2 -. x1 in                                          (*calculate the width of line*)
    P.empty
    >> P.sub  (true_size 0.0     0.0 x1 y1 cols rows)                  (*from left*)
    >> P.line (true_size x_delta 0.0 x1 y1 cols rows)                  (*to right*)

  let draw_long_if_parent_higher x1 y1 x2 y2 cols rows =               (*draws a long line if parent is higher*)
    let x_delta = x2 -. x1 in                                          (*calculate the width of line*)
    let y_delta = y2 -. y1 in                                          (*calculate the height of line*)
    let sz x_draw y_draw x = true_size x_draw y_draw x y1 cols rows in (*the size and position*)
    P.empty
    >> P.sub    (sz 0.0 0.0 x1)
    >> P.line   (sz (x_delta -. 1.0) 0.0 x1 )                          (*we draw the straight line to input for x_delta-1 *)
    >> P.sub    (sz 0.0 0.0 (x2 -. 1.0))                               (*line starts with a straight part from bottom left*)
    >> P.line   (sz 0.4 0.0 (x2 -. 1.0))                               (*to bottom center*)
    >> P.ccurve (sz 0.4 0.0 (x2 -. 1.0))                               (*a little curve*)
                (sz 0.5 0.0 (x2 -. 1.0))
                (sz 0.5 (0.1 *. y_delta) (x2 -. 1.0))
    >> P.line   (sz 0.5 (0.9 *. y_delta) (x2 -. 1.0))                  (*then a line to center top*)
    >> P.ccurve (sz 0.5 (0.9 *. y_delta) (x2 -. 1.0))                  (*a little curve*)
                (sz 0.5 y_delta (x2 -. 1.0))
                (sz 0.6 y_delta (x2 -. 1.0))
    >> P.line   (sz 1.0 y_delta (x2 -. 1.0))                           (*and a line to top right*)

  let draw_long_if_child_higher x1 y1 x2 y2 cols rows =                (*draws a long line if child is higher*)
    let x_delta = x2 -. x1 in                                          (*calculate the width of line*)
    let y_delta = y1 -. y2 in                                          (*calculate the height of line*)
    let sz x_draw y_draw x y = true_size x_draw y_draw x y cols rows in
    P.empty
    >> P.sub    (sz 0.0 0.0 x1 y1)                                     (*a straight line to input of length x_delta-1*)
    >> P.line   (sz (x_delta -. 1.0) 0.0 x1 y1)
    >> P.sub    (sz 0.0 y_delta (x2 -. 1.0) y2)                        (*and then a straight line from left top*)
    >> P.line   (sz 0.4 y_delta (x2 -. 1.0) y2)                        (*to center top*)
    >> P.ccurve (sz 0.4 y_delta (x2 -. 1.0) y2)                        (*a little curve*)
                (sz 0.5 y_delta (x2 -. 1.0) y2)
                (sz 0.5 (0.9 *. y_delta) (x2 -. 1.0) y2)
    >> P.line   (sz 0.5 (0.1 *. y_delta) (x2 -. 1.0) y2)               (*a straight line to bottom center*)
    >> P.ccurve (sz 0.5 (0.1 *. y_delta) (x2 -. 1.0) y2)               (*a little curve*)
                (sz 0.5 0.0 (x2 -. 1.0) y2)
                (sz 0.6 0.0 (x2 -. 1.0) y2)
    >> P.line   (sz 1.0 0.0 (x2 -. 1.0) y2)                            (*a straight line to right bottom*)

  let draw_short_if_parent_higher x1 y1 x2 y2 cols rows =              (*draws a short line if parent is higher*)
    let x_delta = x2 -. x1 in
    let y_delta = y2 -. y1 in
    let sz x_draw y_draw = true_size x_draw y_draw x1 y1 cols rows in
    P.empty
    >> P.sub    (sz  0.0              0.0)                             (*line starts with a straight part from bottom left*)
    >> P.line   (sz (0.4 *. x_delta)  0.0)                             (*to bottom center*)
    >> P.ccurve (sz (0.4 *. x_delta)  0.0)                             (*a little curve*)
                (sz (0.5 *. x_delta)  0.0)
                (sz (0.5 *. x_delta) (0.1 *. y_delta))
    >> P.line   (sz (0.5 *. x_delta) (0.9 *. y_delta))                 (*then a line to center top*)
    >> P.ccurve (sz (0.5 *. x_delta) (0.9 *. y_delta))                 (*a little curve*)
                (sz (0.5 *. x_delta) y_delta)
                (sz (0.6 *. x_delta) y_delta)
    >> P.line   (sz x_delta y_delta)                                   (*and a line to top right*)

  let draw_short_if_child_higher x1 y1 x2 y2 cols rows =
    let x_delta = x2 -. x1 in
    let y_delta = y1 -. y2 in
    let sz x_draw y_draw = true_size x_draw y_draw x1 y2 cols rows in
    P.empty
    >> P.sub    (sz 0.0 y_delta)                                       (*a straight line from left top*)
    >> P.line   (sz (0.4 *. x_delta) y_delta)                          (*to center top*)
    >> P.ccurve (sz (0.4 *. x_delta) y_delta)                          (*a little curve*)
                (sz (0.5 *. x_delta) y_delta)
                (sz (0.5 *. x_delta) (0.9 *. y_delta))
    >> P.line   (sz (0.5 *. x_delta) (0.1 *. y_delta))                 (*a straight line to bottom center*)
    >> P.ccurve (sz (0.5 *. x_delta) (0.1 *. y_delta))                 (*a little curve*)
                (sz (0.5 *. x_delta)  0.0)
                (sz (0.6 *. x_delta)  0.0)
    >> P.line   (sz x_delta 0.0)                                       (*a straight line to right bottom*)

  (* draws a line between elements of given x and y. usually x1,y1 is a child and x2,y2 is a parent *)
  let draw_line acc colour x1 y_1 x2 y2 cols rows =                    (*draws a connecting line between elements*)
    let x1 = (if x1 == 1. then 0.5 else x1) in                         (*for proper drowing of line to input*)
    let y1      = y_1 +. 0.5 in                                        (*a line should be a half higher*)
    let area    = `O { P.o with P.width = 0.06 /. rows /. cols } in
    (if y1 = y2                                                        (*if the level is equal*)
     then draw_straight x1 y1 x2 y2 cols rows                          (*draw straight line*)
     else if (x2 -. x1) >= 2.0                                         (*if the line is long*)
     then (if y2 > y1                                                  (*if parent is higher*)
           then draw_long_if_parent_higher x1 y1 x2 y2 cols rows
           else draw_long_if_child_higher x1 y1 x2 y2 cols rows)       (*if child is higher*)
     else if y2 > y1                                                   (*if the line is short*)
     then draw_short_if_parent_higher x1 y1 x2 y2 cols rows            (*if parent is higher*)
     else draw_short_if_child_higher x1 y1 x2 y2 cols rows)            (*if child is higher*)
    |> (fun p -> let img = colour >> I.cut ~area p in                  (*cut the image of line with path p*)
                 I.blend img acc)                                      (*blend it with acc*)

end

module Board = struct

  let rec draw t (acc, acc_top) x y cols rows =
    let font       = Font.{ name = ""; slant = `Normal; weight = `W400; size = 0.012 } in
    let num        = List.length t.ports in
    (* the more children converter has the bigger it is *)
    let height     = if t.typ = TS2IP then (float_of_int num *. 2.) else 1. in
    (*then we must place converter in a right place due to its height*)
    let y          = if t.typ = TS2IP then y -. 0.5 else y in
    (*set the name of a board to list*)
    let acc_top    = setting (Board t) x y y height acc_top in
    let area       = `O {P.o with P.width = 0.04 /. cols /. rows } in
    let board_form =                                                     (*a path to draw a board itself *)
      let sz x_draw y_draw = true_size x_draw y_draw x y cols rows in
      P.empty                                                            (*an element - a part of it*)
      >> P.sub    (sz 0.00  0.15)                                        (*left bottom corner  - top left*)
      >> P.line   (sz 0.00 (height -. 0.15))                             (*left top corner     - top left*)
      >> P.ccurve (sz 0.00 (height -. 0.15))                             (*left top corner     - a curve itself*)
                  (sz 0.00 (height -. 0.10))
                  (sz 0.05 (height -. 0.10))
      >> P.line   (sz 0.95 (height -. 0.10))                             (*right top corner    - top left*)
      >> P.ccurve (sz 0.95 (height -. 0.10))                             (*right top corner    - a curve itself*)
                  (sz 1.00 (height -. 0.10))
                  (sz 1.00 (height -. 0.15))
      >> P.line   (sz 1.00 0.15)                                         (*right bottom corner - top right*)
      >> P.ccurve (sz 1.00 0.15)                                         (*right bottom corner - a curve itself*)
                  (sz 1.00 0.10)
                  (sz 0.95 0.10)
      >> P.line   (sz 0.05 0.10)                                         (*left bottom corner  - right bottom*)
      >> P.ccurve (sz 0.05 0.10)                                         (*left bottom corner  - a curve itself*)
                  (sz 0.00 0.10)
                  (sz 0.00 0.15)
    in
    (* a color of a board depends on its state, a color of border is the same but brighter *)
    let colour, border_col =                                             (*a color of board and border*)
      match t.connection with                                            (*according to its state*)
      | `Fine        -> Color.v 0.7 1.0 0.7 1.0, Color.v 0.1 1.0 0.1 1.0 (*light green and green*)
      | `No_response -> Color.v 1.0 0.7 0.7 1.0, Color.v 1.0 0.1 0.1 1.0 (*light red and red*)
      | `Init        -> Color.v 0.8 0.8 0.8 1.0, Color.v 0.2 0.2 0.2 1.0 (*light gray and gray*)
    in
    let border      = I.const border_col >> I.cut ~area board_form in    (*draw border*)
    let board       = I.const colour  >> I.cut board_form in             (*draw board*)
    let with_border = I.blend border board in                            (*and image of board and border*)
    let with_label  = I.blend (text ~text:(board_to_string t.typ)        (*an image with label*)
                                    (true_size 0.1 0.05 x (y +. height -. 1. +. 0.42) cols rows)
                                    { font with size = (0.15 /. cols); weight = `W400 })
                              with_border in
    let with_model  = I.blend (text ~text:t.model                        (*an image with model*)
                                    (true_size 0.1 0.05 x (y +. height -. 1. +. 0.27) cols rows)
                                    { font with size = (0.13 /. cols); weight = `W100 })
                              with_label in
    let with_man    = I.blend (text ~text:t.manufacturer                 (*an image with manufacturer*)
                                    (true_size 0.1 0.05 x (y +. height -.1. +. 0.15) cols rows)
                                    { font with size = (0.13 /.cols); weight = `W100 })
                              with_model in
    let img1        = I.blend with_man acc in                            (*blended with acc*)
    match t.ports with
    | [] -> (img1,acc_top)                                               (*if ports list is empty*)
    | l  ->
       CCList.foldi (fun (acc,acc_top) i z ->
           let colour1 = if z.listening then green else gray in          (*a color of line due to state "listening"*)
           match t.typ with                                              (*so if board type is*)
           | TS2IP ->
              let y_child = y +. float_of_int ((num-i-1)*2) in           (*converter, then child height is <-*)
              let draw_line x1 = (Port.draw_line acc colour1 x1 y_child x (y_child +. 0.5) cols rows),
                                 acc_top in                              (*we blend the image of line with acc_top*)
              (match z.child with                                        (*then we decide what to draw - input or board*)
               | Input el -> Port.draw_input el (draw_line 1.0) 0.0 y_child cols rows
               | Board el -> draw el (draw_line @@ x -. 1.0) (x -. 2.0) y_child cols rows)
           | _ ->
              let y_child =                                              (*if it's not a converter,*)
                match num with                                           (*this part is used for calculating*)
                | 1 -> y                                                 (*the height of the child*)
                | _ -> match num mod 2 with
                       | 1 -> if i <= num/2 then y +. float_of_int (num/2 - i) else y +. float_of_int (num/2 - i)
                       | _ -> if i <= num/2 then y +. float_of_int (num/2 - i) -. 0.5 else y +. float_of_int (num/2 - i - 1) +. 0.5 in
              let y2 = y +. 1.0 -. (float_of_int (i + 1)) /. ((float_of_int num) +. 1.0) in
              (match z.child with
               | Input el ->
                  let line = (Port.draw_line acc colour1 1.0 y_child x y2 cols rows),
                             acc_top in
                  Port.draw_input el line 0.0 y_child cols rows
               | Board el ->
                  let line = (Port.draw_line acc colour1 (x -. 1.0) y_child x y2 cols rows),
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
      | Input x -> Port.draw_input x (acc, acc_top) 0.0 (num +. h) cols_f rows_f
      | Board x -> Board.draw x (acc, acc_top) (cols_f -. 1.0) (num +. h /. 2.) cols_f rows_f
    in
    (acc1, acc_top1, num +. h)

end

let render ?on_click ~topology ~(width : int) ~canvas () =
  let t      = topology in
  let cols   = (get_list_depth t * 2 - 1) in
  let rows   = (get_list_height t) in
  let ar     = 1. in
  let cw     = let calc = width / cols in
               if calc > 150 then 150 else calc in
  let rh     = int_of_float @@ (float_of_int cw) /. ar in
  let start  = I.const @@ Color.v 1. 1. 1. 0.
               >> I.cut (P.empty
                         >> P.sub  (P2.v 0. 0.)
                         >> P.line (P2.v 0. 1.)
                         >> P.line (P2.v 1. 1.)
                         >> P.line (P2.v 1. 0.)
                         >> P.line (P2.v 0. 0.)) in
  let draw_entry = (fun (acc_img,acc_top,number) x ->
      Entry.draw x (acc_img, acc_top, number) cols rows (float_of_int @@ get_node_height 0 x)) in
  let (acc_img, acc_top, _) = List.fold_left draw_entry (start,[],-0.5) t in
  render canvas (size (cw * cols) (rh * rows)) acc_img;
  let get_node e =
    let x, y  = int_of_float @@ Js.float_of_number @@ (Js.Unsafe.get e "offsetX"),
                int_of_float @@ Js.float_of_number @@ (Js.Unsafe.get e "offsetY") in
    let x1,y1 = floor_to_five @@ (float_of_int x /. float_of_int cw),
                (float_of_int rows) -. (ceil_to_five @@ (float_of_int y /. float_of_int rh)) in
    (match CCList.Assoc.get (x1,y1) acc_top with
     | None       -> None
     | Some entry ->
        match entry with
        | Input _ -> let real_y = float_of_int rows -. float_of_int y /. float_of_int rh in
                     if ((CCList.Assoc.get (x1, y1 +. 0.5) acc_top = Some entry) && (real_y < y1 +. 0.25)
                         || (CCList.Assoc.get (x1, y1 -. 0.5) acc_top = Some entry) && (real_y > y1 +. 0.25))
                     then None
                     else Some entry;
        | Board _ -> Some entry) in
  Dom_events.listen canvas
                    Dom_events.Typ.mousemove
                    (fun _ e -> (match get_node e with
                                 | Some _ -> "pointer"
                                 | None   -> "default")
                                |> (fun x -> (Js.Unsafe.coerce canvas##.style)##.cursor := Js.string x);
                                false)
  |> ignore;
  match on_click with
  | Some f -> Dom_events.listen canvas
                                Dom_events.Typ.click
                                (fun _ e -> (match get_node e with
                                             | Some node -> f node;
                                             | None      -> ());
                                            false)
              |> ignore
  | None -> ()
(*          ;
  Dom_events.listen Dom_html.window
                    Dom_events.Typ.resize
                    (fun _ _ ->
                      let width =
                                  |> Js.Opt.to_option |> CCOpt.get_exn
                                  |> Js.Unsafe.coerce
                                  |> (fun x -> x##.offsetWidth) in
                      let depth = get_list_depth t * 2 - 1 in
                      let real_width = if width/depth > 150 then depth*150 else width in
                      render canvas (size real_width (rh * rows)) acc_img;
                      true)
  |> ignore
 *)
