open Containers
open Components

let insert s (container:#Dom.node Js.t) =
  React.S.map (function
               | Some p -> Dom.list_of_nodeList @@ container##.childNodes
                           |> List.iter (fun x -> Dom.removeChild container x);
                           Dom.appendChild container p#root
               | None   -> ()) s

class type t =
  object inherit Widget.widget
    method on_load   : unit
    method on_unload : unit
  end

let pages =
  let open Tabs in
  let tab f () = ((f ()) :> t) in
  let tab_pages =
    [ "Видео",             tab Mosaic.page
    ; "Редактор",          tab Mosaic_settings.page
    ; "Выбор программ",    tab Structure_settings.page
    ; "Настройки анализа", tab Analysis_settings.page
    ; "Графики",           tab Charts.page
    ]
  in
  let tabs = List.map (fun x -> { content  = `Text (fst x)
                                ; disabled = false
                                ; href     = None
                                ; value    = (snd x) }) tab_pages in
  tabs
