open Js_of_ocaml

let log (x : 'a) : unit =
  Js.Unsafe.global##.console##log x

let intersperse f l =
  let rec aux_direct i f l = match l with
    | [] -> []
    | [_] -> l
    | _ when i=0 -> aux_tailrec [] f l
    | y :: tail -> y :: (f ()) :: aux_direct (i-1) f tail
  and aux_tailrec acc f l = match l with
    | [] -> List.rev acc
    | [y] -> List.rev (y::acc)
    | y :: tail -> aux_tailrec ((f ()) :: y :: acc) f tail
  in
  aux_direct 1_000 f l

let onload _ =
  let root = Dom_html.getElementById "root" in
  let page = Components.Scaffold.attach root in
  let widgets =
    [ (Tree.make ())#widget
    ; (Dgrid.make ())#widget
    ; (Snackbar.section ())#widget
    ; (Slider.section ())#widget
    ; (Checkbox.section ())#widget
    ; (Tabs.section ())#widget
    ; (Dialog.section ())#widget
    ; (Textfield.section ())#widget
    ; (Hexdump.section ())#widget
    ; (Select.section ())#widget ] in
  List.iter (fun x -> x#add_class "demo-section") widgets;
  let div =
    Components.Widget.create_div
      ~widgets:(intersperse (fun () -> (Components.Divider.make ())#widget) widgets)
      () in
  page#set_body div;
  Js._false

let () =
  Dom_html.addEventListener Dom_html.document
    Dom_events.Typ.domContentLoaded
    (Dom_html.handler onload)
    Js._false
  |> ignore
