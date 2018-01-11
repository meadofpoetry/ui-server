open Board_types
open Components

let page control =
  let open Lwt_result.Infix in
  let div = Dom_html.createDiv Dom_html.document in
  let t   =
    Requests.get_config control
    >>= (fun init ->
      let event,sock = Requests.get_config_ws control in
      Lwt_result.return sock)
  in
  div,(fun () -> t >>= (fun -> x##close; Lwt_result.return ()) |> ignore)
