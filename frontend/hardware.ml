open Lwt.Infix
open Hardware_js
open Common.Topology
open Components

let insert s (container:#Dom.node Js.t) =
  React.S.map ~eq:(==)
              (function
               | Some f -> let el,free = f () in
                           Dom.list_of_nodeList @@ container##.childNodes
                           |> CCList.iter (fun x -> Dom.removeChild container x);
                           Dom.appendChild container el;
                           Some free
               | None   -> None) s

let settings_section s =
  let title  = new Typography.Text.t ~font:Headline ~text:"Настройки" () in
  let cont   = Widget.create @@ Dom_html.createDiv Dom_html.document in
  let box    = new Box.t ~widgets:[title#widget; cont#widget] () in
  let s_free = insert s cont#root in
  let _      = React.S.diff (fun _ free -> CCOpt.iter (fun f -> f ()) free) s_free in
  box

let () =
  let doc = Dom_html.document in
  let ac  = Dom_html.getElementById "arbitrary-content" in

  let s,push = React.S.create ~eq:(==) None in
  let divider = new Divider.t () in
  divider#style##.margin := Js.string "15px 0";
  let canvas = Dom_html.createCanvas doc in
  let width () = canvas##.parentNode
                 |> Js.Opt.to_option |> CCOpt.get_exn
                 |> Js.Unsafe.coerce
                 |> (fun x -> x##.offsetWidth) in
  Dom.appendChild ac canvas;
  Dom.appendChild ac divider#root;
  Dom.appendChild ac (settings_section s)#root;
  Requests.get_topology ()
  >>= (fun resp ->
    match resp with
    | Ok t    ->
       let f b = match b.typ with
         | DVB   -> (fun () -> Board_dvb_niit_js.Settings.page b.control)
         | IP2TS -> (fun () -> Board_ts2ip_niit_js.Settings.page b.control)
         | TS2IP -> (fun () -> Board_ip_dektec_js.Ip_dektec.page b.control)
         | TS    -> (fun () -> Board_qos_niit_js.Settings.page b.control)
       in
       Topology.render ~topology:t
                       ~canvas
                       ~width:(width ())
                       ~on_click:(function
                                  | Input _ -> ()
                                  | Board b -> push (Some (f b)))
                       ()
       |> Lwt.return
    | Error e -> Lwt.return @@ print_endline e)
  |> ignore;
  React.E.map (fun x -> Topology.render ~topology:x ~canvas ~width:(width ()) ())
              (fst (Requests.get_topology_socket ()))
  |> ignore
