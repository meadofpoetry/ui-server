open Containers
open Components

module Requests = Requests
module Ui       = Ui

let insert s (container:#Dom.node Js.t) =
  React.S.map (function
               | Some p -> Dom.list_of_nodeList @@ container##.childNodes
                           |> List.iter (fun x -> Dom.removeChild container x);
                           Dom.appendChild container p#root
               | None   -> ()) s

let pages =
  let open Tabs in
  let tab f () = ((f ()) :> Widget.t) in
  let tab_pages =
    [ "Видео",             tab Mosaic.page
    ; "Редактор",          tab Wm_page.page
    (* ; "Настройки анализа", tab Analysis_settings.page
     * ; "Графики",           tab Charts.page *)
    ]
  in
  let tabs = List.map (fun x -> { content  = `Text (fst x)
                                ; disabled = false
                                ; href     = None
                                ; value    = (snd x) }) tab_pages in
  tabs
