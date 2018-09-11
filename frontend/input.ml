open Containers
open Components
open Api_js.Requests
open Lwt.Infix
open Common.Topology

let dummy_tab = fun () ->
  let div = Dom_html.createDiv Dom_html.document in
  Ui_templates.Placeholder.under_development ()

let board_to_tabs input (control : int) = function
  | "IP2TS" ->
     [ "IP", "ip",
       (fun () -> Board_ip_dektec_js.Input_page.make control
                  |> Widget.coerce) ]
  | "DVB" ->
     [ "RF", "rf",
       (fun () -> Board_dvb_niit_js.Input_page.make control
                  |> Widget.coerce) ]
  | "TS" ->
     [ "QoS", "qos",
       (fun () -> Board_qos_niit_js.Streams_page.make input control)
     ]
  | "TS2IP" -> [ ]
  | s       -> failwith ("input.js: unknown board " ^ s)

let cpu_to_tabs  = function
  | "pipeline" -> [ "QoE", "qoe", dummy_tab ]
  | _          -> []

let append_maybe x l = match x with None -> l | Some x -> l @ x

let tabs () =
  let input =
    Js.Unsafe.global##.input
    |> Js.to_string
    |> Show_topo_input.of_string in
  let boards =
    Yojson.Safe.from_string @@ Js.to_string
    @@ Json.output @@ Js.Unsafe.global##.boards
    |> boards_of_yojson
    |> Result.get_exn in
  let cpu =
    Yojson.Safe.from_string @@ Js.to_string
    @@ Json.output @@ Js.Unsafe.global##.cpu
    |> cpu_opt_of_yojson
    |> Result.get_exn in
  let tabs =
    List.fold_left (fun acc (control, typ) ->
        (List.rev @@ board_to_tabs input control typ) @ acc) [] boards
    |> List.rev
    |> append_maybe (Option.map cpu_to_tabs cpu)
    |> List.map (fun (name, hash, f) ->
           new Tab.t ~content:(Text name) ~value:(hash, f) ())
  in tabs

let () =
  let _ = new Ui_templates.Page.t (`Dynamic (tabs ())) () in
  ()

