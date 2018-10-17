open Containers
open Lwt_react
open Board_ip_dektec_js.Requests
open Application_js.Requests
open Components

let onload _ = Js._false

let () = Dom_html.addEventListener Dom_html.document
           Dom_events.Typ.domContentLoaded
           (Dom_html.handler onload)
           Js._false
         |> ignore
