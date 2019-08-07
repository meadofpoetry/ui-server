open Js_of_ocaml
open Components
open Board_niitv_dvb_widgets

let ( >>= ) = Lwt.bind

module Chart = Widget_chart.Make(struct
    include Util_json.Int
    let equal = ( = )
  end)

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  Lwt.async (fun () ->
      scaffold#loaded
      >>= fun () ->
      (* FIXME *)
      let body = Dom_html.getElementById "rf-tabpanel" in
      let config =
        { Chart.
          sources = []
        ; typ = `Power
        ; settings =
            { range = None
            ; period = `Realtime (Time.Span.of_int_s 20)
            }
        } in
      let chart = Chart.make [] config in
      chart#root##.style##.height := Js.string "300px";
      chart#root##.style##.padding := Js.string "1rem";
      chart#add_class Card.CSS.root;
      Dom.appendChild body chart#root;
      Lwt.return_unit)
