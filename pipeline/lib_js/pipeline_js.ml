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
  let tab f () = ((f ()) :> Widget.t) in
  [ new Tab.t ~content:(Text "Видео") ~value:(tab Mosaic.page) ()
  ; new Tab.t ~content:(Text "Редактор") ~value:(tab Wm_page.page) ()
  ]
