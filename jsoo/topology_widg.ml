open Gg
open Vg
open Tyxml

let size = Size2.v 300.0 300.0
let view = Box2.unit
let box =
  let sz = Box2.v P2.o (Size2.v 0.04 0.04) in
  let sq = P.empty >> P.rect sz in
  I.const Color.blue >> I.cut sq

let image =
  let mark img pt = img >> I.move pt in
  let blend acc (pt, img) = acc >> I.blend (mark img pt) in
  List.fold_left blend I.void 
                 [(V2.v 0.25 0.25), box;
                  (V2.v 0.50 0.50), box;
                  (V2.v 0.75 0.25), box;]

let scatter_plot pts pt_width =
  let dot =
    let circle = P.empty >> P.circle P2.o (0.5 *. pt_width) in
    I.const Color.black >> I.cut circle
  in
  let mark pt = dot >> I.move pt in
  let blend_mark acc pt = acc >> I.blend (mark pt) in
  List.fold_left blend_mark I.void pts
        
module Topology = struct

  let base_class = "mdc-card"

  let render canvas _ =
    let r = Vgr.create (Vgr_htmlc.target canvas) `Other in   (* 4 *)
    ignore (Vgr.render r (`Image (size, view, image))); (* 5 *)
    ignore (Vgr.render r `End)

  let create doc t = 
    let div = Dom_html.createDiv   doc in
    let h2 = Dom_html.createH2     doc in
    let c  = Dom_html.createCanvas doc in
    h2##.textContent := Js.some @@
                          Js.string ("rendered: " ^ (Yojson.Safe.to_string @@ Common.Hardware.topology_to_yojson t));
    Dom.appendChild div h2;
    Dom.appendChild div c;
    render c t;
    let (x0, y0), (w, h) =
      (c##.clientLeft, c##.clientTop), (c##.clientWidth, c##.clientHeight)
    in 
    Printf.printf "%d %d %d %d\n" x0 y0 w h;
    c##.onclick := Dom.handler
                     (fun ev -> let (x0, y0), (w, h) =
                                  (c##.offsetLeft, c##.offsetTop), (c##.offsetWidth, c##.offsetHeight)
                                in 
                                Printf.printf "%d %d %d %d\n" x0 y0 w h;
                                Printf.printf "Mouse clicked on %d %d\n"
                                              ev##.clientX ev##.clientY;
                                Js._false);
    div

end
