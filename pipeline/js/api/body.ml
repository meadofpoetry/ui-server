open Js_of_ocaml
open Js_of_ocaml.WebSockets

include Application_types.Body
let of_event evt = of_string @@ Js.to_string evt##.data
