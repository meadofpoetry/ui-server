open Gg
open Vg
open Tyxml

type element = { img   : I.t React.signal
               ; sz    : size2
               ; click : unit -> unit
               }
  
module type ELEMENT = sig
  type t
  val create : ?sz:size2 -> t -> element
end
                    
module Board_element : ELEMENT with type t := Common.Hardware.topo_board = struct
  open Common.Hardware
  let create ?(sz = Size2.v 0.04 0.02) b =
    let rect =
      let box = Box2.v P2.o sz in
      I.const Color.blue >> I.cut (P.empty >> P.rect box)
    in
    { img   = React.S.const rect
    ; sz    = sz
    ; click = (fun () -> Printf.printf "%d was clicked\n" b.control)
    }
end

module Input_element : ELEMENT with type t := Common.Hardware.input = struct
  open Common.Hardware
  let create ?(sz = Size2.v 0.03 0.01) i =
    let is = match i with
      | RF -> "RF"
      | TSOIP -> "TSOIP"
      | ASI -> "ASI"
    in 
    let rect =
      let box = Box2.v P2.o sz in
      I.const Color.red >> I.cut (P.empty >> P.rect box)
    in
    { img   = React.S.const rect
    ; sz    = sz
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
                                                                    ; active = true
                                                                    ; ports = []});
              V2.v 0.75 0.75, Input_element.create Common.Hardware.(RF)]

  let mark img pt = img >> I.move pt
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
    let merge img (pt, elem) = blend img (pt, React.S.value elem.img) in
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
