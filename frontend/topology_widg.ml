open Gg
open Vg
open Tyxml

type element = { img   : I.t
               ; sz    : size2
               ; click : unit -> unit
               }

let font    = Font.{ name = "Roboto+Mono"; slant = `Normal; weight = `W800; size = 1. }

let color_back  = (Color.v_srgbi 230 230 230)

let color_ok  = (Color.v_srgbi 102 204 145)
                
let color_err = (Color.v_srgbi 255 55 46)

let draw_input ~text () =
  let size = Size2.v 1. 0.75 in
  let black = I.const Color.black in
  let back  = I.const color_back in
  let box   = P.empty >> P.rect (Box2.v P2.o size) in
  let area  = `O { P.o with P.width = 0.002 } in
  let border = I.cut ~area box black in
  let filling = I.cut box back in
  let text    = I.cut_glyphs ~text { font with size = font.size /. 4. } [] black
                >> I.scale (V2.v 2. 1.) in
  (filling
   >> I.blend border
   >> I.blend text)
  , size
                    
let draw_board ~header ~text ~state () =
  ignore header;
  let size = Size2.v 1. 0.75 in
  let black = I.const Color.black in
  let indc  = I.const (match state with `Ok -> color_ok | `Err -> color_err) in
  let back  = I.const color_back in
  let box   = P.empty >> P.rect (Box2.v P2.o size) in
  let area  = `O { P.o with P.width = 0.002 } in
  let cir   = P.empty >> P.circle (P2.v 0.0 0.0) 0.05 in
  let indbor = I.cut ~area:area cir black in
  let ind   = I.cut cir indc in
  let border = I.cut ~area:area box black in
  let filling = I.cut box back in
  let text    = I.cut_glyphs ~text { font with size = font.size /. 4. } [] black
                >> I.scale (V2.v 2. 1.)  in
  let status  = I.cut_glyphs ~text:"status:" { font with size = font.size /. 8. } [] black
                >> I.scale (V2.v 2. 1.)  in
  let indicator = ind >> I.blend indbor in
  let board     = filling >> I.blend border in
  (board
   >> I.blend (I.move (V2.v 0.25 0.25) text)
   >> I.blend (I.move (V2.v 0.62 0.07) status)
   >> I.blend (I.move (V2.v 0.9 0.1) indicator))
  , size

module type ELEMENT = sig
  type t
  val create : t -> element
end
                    
module Board_element : ELEMENT with type t := Common.Hardware.topo_board = struct
  open Common.Hardware
     
  let create b =
    let (rect, size) = draw_board ~header:"board" ~text:"1" ~state:`Ok () in
    { img   = rect
    ; sz    = size
    ; click = (fun () -> Printf.printf "%d was clicked\n" b.control)
    }
end

module Input_element : ELEMENT with type t := Common.Hardware.input = struct
  open Common.Hardware
  let create i =
    let is = match i with
      | RF -> "RF"
      | TSOIP -> "TSOIP"
      | ASI -> "ASI"
    in 
    let (rect, size) = draw_input ~text:is () in
    { img   = rect
    ; sz    = size
    ; click = (fun () -> Printf.printf "%s was clicked\n" is)
    }
end                                                                        
  
module Topology = struct

  let base_class = "mdc-card"

  let size = Size2.v 300.0 300.0
  let view = Box2.unit

  let test = [V2.v 0.25 0.25, Board_element.create Common.Hardware.({ typ = (Converter IP)
                                                                    ; model = ""
                                                                    ; manufacturer = ""
                                                                    ; version = 42
                                                                    ; control = 1
                                                                    ; connection = `Fine
                                                                    ; ports = []});
              V2.v 0.75 0.75, Input_element.create Common.Hardware.(RF)]

  let mark img pt = img >> I.scale (V2.v 0.3 0.3) >> I.move pt
  let blend acc (pt, img) = acc >> I.blend (mark img pt)

  let merge_elems c =
    let cb = List.map (fun (pt, elt) ->
                 let p width height x y =
                   let x = float_of_int x /. float_of_int width in
                   let y = float_of_int (height - y) /. float_of_int height in
                   let elx = V2.x pt in
                   let ely = V2.y pt in
                   let dx = Size2.w elt.sz in
                   let dy = Size2.h elt.sz in
                   x >= elx && x <= (elx +. dx) && y >= ely && y <= (ely +. dy)
                 in p, elt.click) test
    in
    let merge img (pt, elem) = blend img (pt, elem.img) in
    c##.onclick := Dom.handler
                     (fun ev -> let w, h = (c##.offsetWidth, c##.offsetHeight) in
                                let x, y = int_of_float @@ Js.float_of_number @@ (Js.Unsafe.get ev "offsetX"),
                                           int_of_float @@ Js.float_of_number @@ (Js.Unsafe.get ev "offsetY") in
                                begin try
                                    let _, clc = List.find (fun (p, _) -> p w h x y) cb in
                                    clc ()
                                  with _ -> print_endline "No callback"
                                end;
                                Js._false);
    List.fold_left merge I.void test

  let render canvas image =
    let r = Vgr.create (Vgr_htmlc.target canvas) `Other in   (* 4 *)
    ignore (Vgr.render r (`Image (size, view, image))); (* 5 *)
    ignore (Vgr.render r `End)

  let create doc _ = 
    let div = Dom_html.createDiv   doc in
    let h2 = Dom_html.createH2     doc in
    let c  = Dom_html.createCanvas doc in
    (*h2##.textContent := Js.some @@
                          Js.string ("rendered: " ^ (Yojson.Safe.to_string @@ Common.Hardware.topology_to_yojson t));*)
    Dom.appendChild div h2;
    Dom.appendChild div c;
    render c (merge_elems c);
    div

end
