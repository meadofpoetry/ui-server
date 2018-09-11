open Containers

module Line = Line
module Bar = Bar
module Pie = Pie

let register_empty_state_plugin text =
  let chart = Js.Unsafe.global##.Chart in
  let after_draw = fun chart ->
    if Float.equal 0. (Js.float_of_number @@ chart##.data##.datasets##.length)
    then begin
        let ctx : Dom_html.canvasRenderingContext2D Js.t =
          chart##.chart##.ctx in
        let width = Js.float_of_number chart##.chart##.width in
        let height = Js.float_of_number chart##.chart##.height in
        let text_width = width /. 2. in
        let text_height = height /. 2. in
        let font_family = "'Helvetica'" in
        let font = Printf.sprintf "%dpx %s" 16 font_family in
        chart##clear ();

        ctx##save;
        ctx##.textAlign := Js.string "center";
        ctx##.textBaseline := Js.string "middle";
        ctx##.font := Js.string font;
        ctx##.fillStyle := Js.string "rgba(100,100,100,1.0)";
        ctx##fillText (Js.string text) text_width text_height;
        ctx##restore;
      end in
  let obj =
    let cb = after_draw in
    Js.Unsafe.obj [| "afterDraw", Js.Unsafe.inject cb |] in
  chart##.plugins##register obj |> ignore
