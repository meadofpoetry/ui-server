open Lwt_react
open Requests
open Components

open Lwt.Infix

let insert s (container:#Dom.node Js.t) =
  React.S.map (function
               | Some p -> Dom.list_of_nodeList @@ container##.childNodes
                           |> CCList.iter (fun x -> Dom.removeChild container x);
                           Dom.appendChild container p#root
               | None   -> ()) s

let load () =

  let container = Dom_html.getElementById "arbitrary-content" in

  let box = new Box.t ~widgets:[] () in

  let open Tabs in
  let tab_pages =
    [ "Мозаика",           (fun () -> (Mosaic.page ())#widget)
    ; "Выбор программ",    (fun () -> (Structure_settings.page ())#widget)
    ; "Настройки мозаики", (fun () -> (Mosaic_settings.page ())#widget)
    ; "Настройки анализа", (fun () -> (Analysis_settings.page ())#widget)
    ; "Графики",           (fun () -> (Charts.page ())#widget)
    ]
  in
  let tabs = CCList.map (fun x -> { content  = `Text (fst x)
                                  ; disabled = false
                                  ; href     = None
                                  ; value    = (snd x) }) tab_pages in
  let bar  = new Tabs.Tab_bar.t ~tabs () in
  let s    = React.S.map (function
                          | Some x -> Some (x#get_value ())
                          | None   -> None) bar#s_active in
  let _    = insert s container in

  let section = new Toolbar.Row.Section.t ~align:`Start ~widgets:[bar] () in
  let row     = new Toolbar.Row.t ~sections:[section] () in
  let toolbar = Dom_html.getElementById "main-toolbar" in
  let content = Dom_html.getElementById "main-content" in
  bar#style##.marginLeft := Js.string "72px";
  content##.style##.marginTop := Js.string "128px";
  bar#set_indicator_accent;
  (Js.Unsafe.coerce row#style)##.alignItems := Js.string "flex-end";
  (Js.Unsafe.coerce section#style)##.alignItems := Js.string "flex-end";

  Dom.appendChild toolbar   row#root;
  Dom.appendChild container box#root
