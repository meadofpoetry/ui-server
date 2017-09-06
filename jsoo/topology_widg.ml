open Gg
open Vg
open Tyxml

let aspect = 1.618 
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)
let image = I.const (Color.v_srgb 0.314 0.784 0.471)

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
    div

end
