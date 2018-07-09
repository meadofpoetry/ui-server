open Containers

module Markup = Split.Markup

class t panel1 panel2 () =
object(self)
  inherit Split.t ~vertical:true panel1 panel2 ()

  method private _calc_percent (e:Dom_html.mouseEvent Js.t) =
    let height  = self#client_height in
    let top     = self#offset_top in
    let offsets = Utils.sum_scroll_offsets self#root in
    let rel_y   = (e##.clientY - top) + (snd offsets) in
    (100. *. ((float_of_int rel_y) /. (float_of_int height)))
end
