open Js_of_ocaml

include Application_types.Body
let of_event evt = of_string @@ Js.to_string evt##.data
