open Containers
open Components
open Api_js.Requests
open Lwt.Infix
open Common.Topology

class type t =
  object inherit Widget.widget
    method on_load   : unit
    method on_unload : unit
  end

let board_to_tabs control = function
  | "IP2TS" -> [ "IP",        Board_ip_dektec_js.Ip_dektec.page control ]
  | "DVB"   -> [ "RF",        Board_dvb_niit_js.Measures.page control ]
  | "TS"    -> [ "Структура", Board_qos_niit_js.Structure.page control ]
  | "TS2IP" -> [ ]
  | s       -> failwith ("input.js: unknown board " ^ s)

let tabs () =
  let open Tabs in
  let boards = Yojson.Safe.from_string @@ Js.to_string @@ Json.output @@ Js.Unsafe.variable "boards"
               |> boards_of_yojson
               |> Result.get_exn
  in
  let tabs = List.fold_left (fun acc (c,typ) -> (List.rev @@ board_to_tabs c typ) @ acc) [] boards
             |> List.rev
             |> List.map (fun (name,f) -> { content  = `Text name
                                          ; disabled = false
                                          ; href     = None
                                          ; value    = f })
  in tabs

let () =
  let _ = new Page.t (`Dynamic (tabs ())) () in
  ()

