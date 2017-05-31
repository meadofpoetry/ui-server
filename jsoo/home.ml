(*
let return = Lwt.return
let (>>=) = Lwt.(>>=)

let main () =
  XmlHttpRequest.perform_raw ~override_method:`POST ~response_type:XmlHttpRequest.Text "api/test"
  >>= fun resp ->
  return @@ print_endline (Js.to_bytestring resp.content)
 
let _ =
  Dom_html.window##onload <- Dom_html.handler (fun _ -> ignore (main ()); Js._false)
 *)

let button_type = Js.string "button"

let onload _ =
  let xmlhttp = XmlHttpRequest.create () in
  let doc = Dom_html.document in
  let div = Dom_html.createDiv doc in
  let h2 = Dom_html.createH2 doc in
  let button = Dom_html.createInput ~_type:button_type doc in
  let onreadystatechange () =
    match xmlhttp##.readyState, xmlhttp##.status with
    | XmlHttpRequest.DONE, 200 ->
       let t = xmlhttp##.responseText in
       button##.value := t
    | _ -> ()
  in
  xmlhttp##.onreadystatechange := Js.wrap_callback onreadystatechange;
  xmlhttp##_open (Js.string "POST") (Js.string "api/test") (Js._true);
  h2##.textContent := Js.some (Js.string "Let AJAX change this text");
  button##.value := Js.string "Change Content";
  button##.onclick :=  Dom_html.handler (fun _ -> xmlhttp##send (Js.null); Js._true);

  Dom.appendChild div h2;
  Dom.appendChild doc##.body div;
  Dom.appendChild doc##.body button;
  Js._false

let () = Dom_html.window##.onload := Dom_html.handler onload
