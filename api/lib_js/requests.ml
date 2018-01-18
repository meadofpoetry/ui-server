open Lwt.Infix

let unwrap = Uri.pct_decode

let get addr =
  Lwt_xmlHttpRequest.get addr
  >|= fun frame ->
  if frame.code = 200
  then Ok frame.content
  else Error (Printf.sprintf "Code %d" frame.code)

let get_ok addr =
  Lwt_xmlHttpRequest.get addr
  >|= fun frame ->
  if frame.code = 200
  then Ok ()
  else Error (Printf.sprintf "Code %d" frame.code)

let get_js addr =
  Lwt_xmlHttpRequest.get addr
  >|= fun frame ->
  if frame.code = 200
  then Ok (Yojson.Safe.from_string @@ unwrap frame.content)
  else Error (Printf.sprintf "Code %d" frame.code)

let post_ok addr =
  Lwt_xmlHttpRequest.perform_raw
    ~content_type:"application/json; charset=UTF-8"
    ~headers:["Accept", "application/json, text/javascript, */*; q=0.01";
              "X-Requested-With", "XMLHttpRequest"]
    ~override_method:`POST
    ~response_type:XmlHttpRequest.Text
    addr
  >|= fun frame ->
  if frame.code = 200
  then Ok ()
  else Error (Printf.sprintf "Code %d" frame.code)
  
let post_js addr js =
  let js = Yojson.Safe.to_string js in
  Lwt_xmlHttpRequest.perform_raw
    ~content_type:"application/json; charset=UTF-8"
    ~headers:["Accept", "application/json, text/javascript, */*; q=0.01";
              "X-Requested-With", "XMLHttpRequest"]
    ~override_method:`POST
    ~contents:(`String js)
    ~response_type:XmlHttpRequest.Text
    addr
  >|= fun frame ->
  if frame.code = 200
  then Ok (Yojson.Safe.from_string @@ unwrap @@ Js.to_string frame.content)
  else Error (Printf.sprintf "Code %d" frame.code)

let post_js_ok addr js =
  let js = Yojson.Safe.to_string js in
  Lwt_xmlHttpRequest.perform_raw
    ~content_type:"application/json; charset=UTF-8"
    ~headers:["Accept", "application/json, text/javascript, */*; q=0.01";
              "X-Requested-With", "XMLHttpRequest"]
    ~override_method:`POST
    ~contents:(`String js)
    ~response_type:XmlHttpRequest.Text
    addr
  >|= fun frame ->
  if frame.code = 200
  then Ok ()
  else Error (Printf.sprintf "Code %d" frame.code)

let get_socket addr conv =
  let addr = Js.string @@ Printf.sprintf
                            "ws://%s:8080/%s"
                            (Js.to_string Dom_html.window##.location##.hostname)
                            addr
  in
  let ev, push = React.E.create () in
  let sock = new%js WebSockets.webSocket addr in
  sock##.onmessage := Dom.handler (fun (msg : WebSockets.webSocket WebSockets.messageEvent Js.t)
                                   -> Js.to_string msg##.data
                                      |> Yojson.Safe.from_string
                                      |> conv
                                      |> (function Ok msg -> push msg | Error _ -> ());
                                      Js.bool true);
  ev,sock
