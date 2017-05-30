let return = Lwt.return
let (>>=) = Lwt.(>>=)

module Html = Dom_html

type data = { name  : string
            ; value : float
            }

let main () =
  XmlHttpRequest.perform_raw ~override_method:`POST ~response_type:XmlHttpRequest.Text "api/test"
  >>= fun resp ->
  Lwt_js.yield @@ print_endline (Js.to_bytestring resp.content)

let _ =
  Html.window##onload <- Html.handler (fun _ -> ignore (main ()); Js._false)
