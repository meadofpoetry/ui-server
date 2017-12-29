open Components
open Api_js.Requests
open Lwt.Infix

let get_tabs control =
  get_js (Printf.sprintf "api/board/%d/tabs" control)

let () =
  let controls = Array.to_list @@ Js.to_array @@ Js.Unsafe.variable "boards" in
  let tabs = List.map (fun x -> new Tabs.Tab.t ~text:("Board " ^ (string_of_int x)) ()) controls
             |> (fun tabs -> new Tabs.Tab_bar.t ~tabs ()) in
  let ac  = Dom_html.getElementById "arbitrary-content" in
  Dom.appendChild ac tabs#root

