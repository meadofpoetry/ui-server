open Containers
open Components

type config = unit [@@deriving yojson]

let name = "График"

let make (conf:config option) =
  let _ = Option.get_or ~default:() conf in
  new Hbox.t ~widgets:[] () |> Widget.coerce

