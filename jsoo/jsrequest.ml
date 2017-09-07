open Lwt.Infix

let get addr =
  Lwt_xmlHttpRequest.get addr
  >|= fun frame -> frame.content

let get_js addr =
  Lwt_xmlHttpRequest.get addr
  >|= fun frame ->
  Yojson.Safe.from_string frame.content

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
  Yojson.Safe.from_string @@ Js.to_string frame.content
