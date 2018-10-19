let onload _ = Js._false

let () = Dom_html.addEventListener Dom_html.document
           Dom_events.Typ.domContentLoaded
           (Dom_html.handler onload)
           Js._false
         |> ignore
