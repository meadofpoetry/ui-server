let () =
  Dom_html.addEventListener Dom_html.document
    Dom_events.Typ.domContentLoaded
    (Dom_html.handler Pipeline_js.onload)
    Js._false
  |> ignore
