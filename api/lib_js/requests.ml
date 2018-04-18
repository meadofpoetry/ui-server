open Lwt.Infix

type 'a err = [ `Data of 'a | `String of string ]
   
let unwrap = Uri.pct_decode

let get addr =
  Lwt_xmlHttpRequest.get addr
  >|= fun frame ->
  if frame.code = 200
  then Ok frame.content
  else Error (`String (Printf.sprintf "Code %d" frame.code))

let get_result_unit from_err addr =
  Lwt_xmlHttpRequest.get addr
  >|= fun frame ->
  if frame.code = 200
  then Ok ()
  else match from_err @@ Yojson.Safe.from_string @@ unwrap frame.content with
       | Ok err  -> Error (`Data err)
       | Error _ -> Error (`String (Printf.sprintf "Code %d" frame.code))

let get_result from from_err addr =
  Lwt_xmlHttpRequest.get addr
  >|= fun frame ->
  if frame.code = 200
  then match (from @@ Yojson.Safe.from_string @@ unwrap frame.content) with
       | Ok _ as v -> v
       | Error e   -> Error (`String e)
  else match (from_err @@ Yojson.Safe.from_string @@ unwrap frame.content) with
       | Ok err  -> Error (`Data err)
       | Error _ -> Error (`String (Printf.sprintf "Code %d" frame.code))

let post_result ?contents addr =
  let contents = match contents with
    | None    -> None
    | Some js -> Some (`String (Yojson.Safe.to_string js))
  in
  Lwt_xmlHttpRequest.perform_raw
    ~content_type:"application/json; charset=UTF-8"
    ~headers:["Accept", "application/json, text/javascript, */*; q=0.01";
              "X-Requested-With", "XMLHttpRequest"]
    ~override_method:`POST
    ?contents
    ~response_type:XmlHttpRequest.Text
    addr
  >|= fun frame ->
  if frame.code = 200
  then Ok ()
  else Error (`String (Printf.sprintf "Code %d" frame.code))
 

let post_result_unit ?contents from_err addr =
  let contents = match contents with
    | None    -> None
    | Some js -> Some (`String (Yojson.Safe.to_string js))
  in
  Lwt_xmlHttpRequest.perform_raw
    ~content_type:"application/json; charset=UTF-8"
    ~headers:["Accept", "application/json, text/javascript, */*; q=0.01";
              "X-Requested-With", "XMLHttpRequest"]
    ~override_method:`POST
    ?contents
    ~response_type:XmlHttpRequest.Text
    addr
  >|= fun frame ->
  if frame.code = 200
  then Ok ()
  else match (from_err @@ Yojson.Safe.from_string @@ unwrap @@ Js.to_string frame.content) with
       | Ok err  -> Error (`Data err)
       | Error _ -> Error (`String (Printf.sprintf "Code %d" frame.code))
  
let post_result ?contents from from_err addr =
  let contents = match contents with
    | None    -> None
    | Some js -> Some (`String (Yojson.Safe.to_string js))
  in
  Lwt_xmlHttpRequest.perform_raw
    ~content_type:"application/json; charset=UTF-8"
    ~headers:["Accept", "application/json, text/javascript, */*; q=0.01";
              "X-Requested-With", "XMLHttpRequest"]
    ~override_method:`POST
    ?contents
    ~response_type:XmlHttpRequest.Text
    addr
  >|= fun frame ->
  if frame.code = 200
  then match (from @@ Yojson.Safe.from_string @@ unwrap @@ Js.to_string frame.content) with
       | Ok _ as v -> v
       | Error e   -> Error (`String e)
  else match (from_err @@ Yojson.Safe.from_string @@ unwrap @@ Js.to_string frame.content) with
       | Ok err  -> Error (`Data err)
       | Error _ -> Error (`String (Printf.sprintf "Code %d" frame.code))

let get_socket addr from =
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
                                      |> from
                                      |> (function Ok msg -> push msg | Error _ -> ());
                                      Js.bool true);
  ev,sock
