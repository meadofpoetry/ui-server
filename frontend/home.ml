let onload _ = Js_of_ocaml.Js._false

let () = Js_of_ocaml.(
    Dom_html.addEventListener Dom_html.document
      Dom_events.Typ.domContentLoaded
      (Dom_html.handler onload)
      Js._false
    |> ignore)
