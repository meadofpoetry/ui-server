open Lwt.Infix

let get addr =
  Lwt_xmlHttpRequest.get addr
  >|= fun frame ->
  if frame.code = 200
  then Ok frame.content
  else Error (Printf.sprintf "Code %d" frame.code)

let get_js addr =
  Lwt_xmlHttpRequest.get addr
  >|= fun frame ->
  if frame.code = 200
  then Ok (Yojson.Safe.from_string frame.content)
  else Error (Printf.sprintf "Code %d" frame.code)

let post_ok addr =
  let post_args = [] in
  Lwt_xmlHttpRequest.perform_raw
    ~content_type:"application/json; charset=UTF-8"
    ~headers:["Accept", "application/json, text/javascript, */*; q=0.01";
              "X-Requested-With", "XMLHttpRequest"]
    ~override_method:`POST
    ~contents:(`POST_form post_args)
    ~response_type:XmlHttpRequest.Text
    addr
  >|= fun frame ->
  if frame.code = 200
  then Ok ()
  else Error (Printf.sprintf "Code %d" frame.code)
  
let post_js addr js =
  let js = Yojson.Safe.to_string js in
  let post_args = ["data", `String (Js.bytestring js)] in
  Lwt_xmlHttpRequest.perform_raw
    ~content_type:"application/json; charset=UTF-8"
    ~headers:["Accept", "application/json, text/javascript, */*; q=0.01";
              "X-Requested-With", "XMLHttpRequest"]
    ~override_method:`POST
    ~contents:(`POST_form post_args)
    ~response_type:XmlHttpRequest.Text
    addr
  >|= fun frame ->
  if frame.code = 200
  then Ok (Yojson.Safe.from_string @@ Js.to_string frame.content)
  else Error (Printf.sprintf "Code %d" frame.code)

let post_js_ok addr js =
  let js = Yojson.Safe.to_string js in
  let post_args = ["data", `String (Js.bytestring js)] in
  Lwt_xmlHttpRequest.perform_raw
    ~content_type:"application/json; charset=UTF-8"
    ~headers:["Accept", "application/json, text/javascript, */*; q=0.01";
              "X-Requested-With", "XMLHttpRequest"]
    ~override_method:`POST
    ~contents:(`POST_form post_args)
    ~response_type:XmlHttpRequest.Text
    addr
  >|= fun frame ->
  if frame.code = 200
  then Ok ()
  else Error (Printf.sprintf "Code %d" frame.code)

let get_socket addr conv =
  let addr = Js.string @@ Printf.sprintf
                            "ws://%s/%s"
                            (Js.to_string Dom_html.window##.location##.hostname)
                            addr
  in
  let ev, push = React.E.create () in
  let sock = new%js WebSockets.webSocket addr in
  sock##.onmessage := Dom.handler (fun (msg : WebSockets.webSocket WebSockets.messageEvent Js.t)
                                   -> let open CCResult in
                                      Js.to_string msg##.data
                                      |> Yojson.Safe.from_string
                                      |> conv
                                      >|= push
                                      |> ignore;
                                      Js.bool true);
  ev
