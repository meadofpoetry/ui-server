open Containers
open Lwt.Infix
open Application_js
open Common.Topology
open Components

let insert s (container:#Dom.node Js.t) =
  React.S.map (function
               | Some p -> Dom.list_of_nodeList @@ container##.childNodes
                           |> List.iter (fun x -> Dom.removeChild container x);
                           Dom.appendChild container p#root
               | None   -> ()) s

let settings_section s =
  let title  = new Typography.Text.t ~font:Headline ~text:"Настройки" () in
  let cont   = Widget.create @@ Dom_html.createDiv Dom_html.document in
  let box    = new Box.t ~widgets:[title#widget; cont#widget] () in
  let _      = insert s cont#root in
  box

let () =
  let doc = Dom_html.document in
  let ac  = Dom_html.getElementById "arbitrary-content" in

  let s,push = React.S.create ~eq:Equal.physical None in
  let divider = new Divider.t () in
  divider#style##.margin := Js.string "15px 0";
  let topo_el = Dom_html.createDiv doc in
  Dom.appendChild ac topo_el;

  Requests.get_topology ()
  >>= (fun resp ->
    match resp with
    | Ok t    ->
       let f b = match b.typ with
         | "DVB"   -> (new Board_dvb_niit_js.Settings.settings b.control ())#widget
         | "TS2IP" -> (new Board_ts2ip_niit_js.Settings.settings b.control ())#widget
         | "IP2TS" -> Widget.create @@ fst @@ Board_ip_dektec_js.Ip_dektec.page b.control
         | "TS"    -> Widget.create @@ fst @@ Board_qos_niit_js.Settings.page b.control
         | s       -> failwith ("Requests.get_topology: unknown board " ^ s)
       in
       Topology.render ~topology:t
                       ~topo_el
                       ~on_click:(function
                                  | Input _ -> ()
                                  | Board b -> push (Some (f b)))
                       ()
       |> Lwt.return
    | Error e -> Lwt.return @@ print_endline e)
  |> ignore;
  React.E.map (fun x -> Topology.render ~topology:x ~topo_el ())
              (fst (Requests.get_topology_socket ()))
  |> ignore
