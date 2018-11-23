module Markup = Split.Markup

class t panel1 panel2 () =
object(self)
  inherit Split.t ~vertical:false panel1 panel2 ()

  method private _calc_percent (e : Dom_html.mouseEvent Js.t) =
    let rect = self#bounding_client_rect in
    let width = self#client_width in
    let offsets = Utils.sum_scroll_offsets self#root in
    let rel_x = (e##.clientX - (int_of_float rect.left)) + (fst offsets) in
    (100. *. ((float_of_int rel_x) /. (float_of_int width)))
end
