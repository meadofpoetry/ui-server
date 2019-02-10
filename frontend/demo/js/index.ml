open Js_of_ocaml
open Components

let log (x : 'a) : unit =
  Js.Unsafe.global##.console##log x

let onload _ =
  let root = Dom_html.getElementById "root" in
  let slider = Slider.make ~discrete:true ~markers:true ~step:5. () in
  let div = Widget.create_div ~widgets:[slider] () in
  div#add_class "slider-wrapper";
  Icon_button.make ~icon:(Icon.SVG.(create_simple Path.access_point)) ()
  |> (fun i -> print_endline @@ Js.to_string i#root##.nodeName);
  let page = Scaffold.attach root in
  page#set_body div;
  slider#layout ();
  Js._false

let () =
  Dom_html.addEventListener Dom_html.document
    Dom_events.Typ.domContentLoaded
    (Dom_html.handler onload)
    Js._false
  |> ignore
