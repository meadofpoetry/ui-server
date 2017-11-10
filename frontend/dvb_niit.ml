

let onload _ =
  let doc = Dom_html.document in
  
  let board_id = int_of_float @@ Js.float_of_number (Js.Unsafe.variable "boardId") in
  let cont = Dom_html.getElementById "dvb_widgets" in

  let label = Dom_html.createP doc in
  label##.textContent := Js.some @@ Js.string ( "Board dvb, id " ^ (string_of_int board_id));
  Dom.appendChild cont label;
  
  Js._false

let () =
  Dom_html.addEventListener Dom_html.document
    Dom_events.Typ.domContentLoaded
    (Dom_html.handler onload)
    Js._false
  |> ignore
