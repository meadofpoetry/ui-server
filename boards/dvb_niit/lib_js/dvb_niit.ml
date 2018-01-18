open Components

type page_state =
  { state_ws    : WebSockets.webSocket Js.t
  ; measures_ws : WebSockets.webSocket Js.t
  }

let free state =
  let open Lwt_result.Infix in
  state >>= (fun x -> x.state_ws##close; x.measures_ws##close; Lwt_result.return ())

let page control =
  let open Lwt_result.Infix in
  let container = Dom_html.createDiv Dom_html.document in
  let t =
    Requests.get_config control
    >>= (fun cfg ->
      Requests.get_state control
      >>= (fun state ->
           let e_state,state_ws       = Requests.get_state_ws control in
           let e_measures,measures_ws = Requests.get_measures_ws control in
           Lwt_result.return { state_ws; measures_ws }))
  in
  container, (fun () -> free t |> ignore)
