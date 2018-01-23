open Components
open Api_js.Requests
open Lwt.Infix
open Common.Topology

let insert s (container:#Dom.node Js.t) =
  React.S.map ~eq:(==)
              (function
               | Some f -> let el,free = f () in
                           Dom.list_of_nodeList @@ container##.childNodes
                           |> CCList.iter (fun x -> Dom.removeChild container x);
                           Dom.appendChild container el;
                           Some free
               | None   -> None) s

let () =
  let open Components.Tabs in
  let box  = new Widget.widget (Tyxml_js.Html.div [] |> Tyxml_js.To_dom.of_element) () in
  let boards = Yojson.Safe.from_string @@ Js.to_string @@ Json.output @@ Js.Unsafe.variable "boards"
               |> boards_of_yojson
               |> CCResult.get_exn in
  let board_to_tabs control = function
    | IP2TS -> [ "IP", (fun () -> Board_ip_dektec_js.Ip_dektec.page control) ]
    | DVB   -> [ "RF", (fun () -> print_endline "new rf page!";
                                  (new Board_dvb_niit_js.Measures.measures control ())#root, (fun () -> ())) ]
    | TS    -> [ "QoS",       (fun () -> Board_qos_niit_js.Settings.page control)
               ; "Структура", (fun () -> Board_qos_niit_js.Structure.page control)
               ]
    | TS2IP -> [ "TS2IP", (fun () -> (new Board_ts2ip_niit_js.Settings.settings control ())#root, (fun () -> ())) ]
  in
  let tabs = CCList.fold_left (fun acc (c,typ) -> (CCList.rev @@ board_to_tabs c typ) @ acc) [] boards
             |> CCList.rev in
  let bar  = new Tabs.Tab_bar.t ~tabs:(CCList.map (fun (name,_) -> { content  = `Text name
                                                                   ; disabled = false
                                                                   ; href     = None
                                                                   ; value    = () })
                                                  tabs) () in
  let s    = React.S.map ~eq:(==) (fun _ -> CCOpt.map (fun idx -> snd @@ CCList.get_at_idx_exn idx tabs)
                                     bar#get_active_tab_index)
                         bar#s_active in
  let s_free  = insert s box#root in
  let _       = React.S.diff (fun _ free -> CCOpt.iter (fun f -> f ()) free) s_free in
  let ac      = Dom_html.getElementById "arbitrary-content" in
  let section = new Toolbar.Row.Section.t ~align:`Start ~widgets:[bar] () in
  let row     = new Toolbar.Row.t ~sections:[section] () in
  let toolbar = Dom_html.getElementById "main-toolbar" in
  let content = Dom_html.getElementById "main-content" in
  bar#style##.marginLeft := Js.string "72px";
  content##.style##.marginTop := Js.string "128px";
  bar#set_indicator_accent;
  (Js.Unsafe.coerce row#style)##.alignItems := Js.string "flex-end";
  (Js.Unsafe.coerce section#style)##.alignItems := Js.string "flex-end";
  Dom.appendChild toolbar row#root;
  Dom.appendChild ac box#root

