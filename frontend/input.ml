open Containers
open Components
open Api_js.Requests
open Lwt.Infix
open Common.Topology

let dummy_tab = fun () -> let div = Dom_html.createDiv Dom_html.document in
                          Widget.create div

let board_to_tabs (control:int) : string -> (string * (unit -> Widget.t)) list = function
  | "IP2TS" -> [ "IP",        (fun () -> Board_ip_dektec_js.Input_page.make control |> Widget.coerce) ]
  | "DVB"   -> [ "RF",        (fun () -> Board_dvb_niit_js.Input_page.make control |> Widget.coerce) ]
  | "TS"    -> [ "QoS",       (fun () -> Board_qos_niit_js.Qos_log.page control () |> Widget.coerce)
               ; "Структура", (fun () -> Board_qos_niit_js.Structure.page control () |> Widget.coerce)
               (* ; "Скорости",  dummy_tab
                * ; "Джиттер",   dummy_tab *)
               ]
  | "TS2IP" -> [ ]
  | s       -> failwith ("input.js: unknown board " ^ s)

let cpu_to_tabs : string -> (string * (unit -> Widget.t)) list = function
  | "pipeline" -> ["QoE", dummy_tab ]
  | _          -> []

let append_maybe x l = match x with None -> l | Some x -> l @ x

let tabs () =
  let open Tabs in
  let boards = Yojson.Safe.from_string @@ Js.to_string @@ Json.output @@ Js.Unsafe.global##.boards
               |> boards_of_yojson
               |> Result.get_exn
  in
  let cpu    = Yojson.Safe.from_string @@ Js.to_string @@ Json.output @@ Js.Unsafe.global##.cpu
               |> cpu_opt_of_yojson
               |> Result.get_exn
  in
  let tabs = List.fold_left (fun acc (c,typ) -> (List.rev @@ board_to_tabs c typ) @ acc) [] boards
             |> List.rev
             |> append_maybe (Option.map cpu_to_tabs cpu)
             |> List.map (fun (name,f) -> { content  = `Text name
                                          ; disabled = false
                                          ; href     = None
                                          ; value    = f })
  in tabs

let () =
  let _ = new Page.t (`Dynamic (tabs ())) () in
  ()

