open React
open Yojson
   
let return = Lwt.return
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let button_type = Js.string "button"

let make_struct () =
  let tmp = Qoe.default in
  Qoe.Qoe_root.to_yojson tmp
  |> Qoe.filter_none
  |> Yojson.Safe.to_string

let onload _ =
  let str = make_struct () in
  
  print_endline str;
  
  let ask_server push =
    let post_args = ["data", `String (Js.bytestring str)] in
    XmlHttpRequest.perform_raw
      ~content_type:"application/json; charset=UTF-8"
      ~headers:["Accept", "application/json, text/javascript, */*; q=0.01";
                "X-Requested-With", "XMLHttpRequest"]
      ~override_method:`POST
      ~post_args
      ~response_type:XmlHttpRequest.Text
      "api/test"
    >|= (fun resp ->
    push @@ Js.some resp.content)
  in
  
  let v, push = S.create Js.null in
  
  let doc = Dom_html.document in
  let div = Dom_html.createDiv doc in
  let h2 = Dom_html.createH2 doc in
  let button_set   = Dom_html.createInput ~_type:button_type doc in
  let button_reset = Dom_html.createInput ~_type:button_type doc in
  button_set##.value := Js.string "Change Content";
  button_reset##.value := Js.string "Reset Content";
    
  let _ = S.map (fun text -> h2##.textContent := text; text) v in

  Lwt.ignore_result @@ Lwt_js_events.clicks button_set (fun _ _ -> ask_server push);
  Lwt.ignore_result @@ Lwt_js_events.clicks button_reset (fun _ _ -> return @@ push Js.null);

  Dom.appendChild div h2;
  Dom.appendChild doc##.body div;
  Dom.appendChild doc##.body button_set;
  Dom.appendChild doc##.body button_reset;
  Js._false

let () = Dom_html.window##.onload := Dom_html.handler onload
