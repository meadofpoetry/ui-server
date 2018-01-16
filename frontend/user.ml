let () =
  let user      = Js.to_string @@ Js.Unsafe.variable "username" in
  let doc       = Dom_html.document in
  let container = Dom_html.getElementById "arbitrary-content" in
  let text      = Dom_html.createP doc in
  text##.textContent := Js.some @@ Js.string user;
  Dom.appendChild container text;
