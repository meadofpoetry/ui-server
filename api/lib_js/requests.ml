open Lwt.Infix

type 'a err      = [ `Data of int * 'a | `Code of int ]
type json        = Yojson.Safe.json
type 'a contents = [ `Blob of (#File.blob Js.t as 'a)
                   | `Form_contents of Form.form_contents
                   | `POST_form of (string * Form.form_elt) list
                   | `String of string
                   ]

let err_to_string : 'a. ?to_string:('a -> string) -> 'a err -> string = fun ?to_string err ->
  match err with
  | `Data (i,a) -> (match to_string with
                    | Some f -> Printf.sprintf "Код %d. %s" i @@ f a
                    | None   -> Printf.sprintf "Код %d" i)
  | `Code i     -> Printf.sprintf "Код %d" i

let unwrap = Common.Uri.pct_decode

module type Req = sig

  type t
  type response

  val content_type  : string
  val accept        : string
  val response_type : response XmlHttpRequest.response
  val parse         : response -> t
  val to_contents   : t -> 'a contents
  val of_socket_msg : WebSockets.webSocket WebSockets.messageEvent Js.t -> t

end

module type WS = sig

  type t

  val create : ?port:int -> string -> WebSockets.webSocket Js.t
  val get    : ?port:int -> string -> (t -> ('a,string) result) -> ('a React.event * WebSockets.webSocket Js.t)

end

module type Request = sig

  type t
  type response

  val get_frame   : string -> response Lwt_xmlHttpRequest.generic_http_frame Lwt.t
  val get         : string -> (t,int) Lwt_result.t
  val get_result  : ?from_err:(t -> ('b,string) result) ->
                    (t -> ('a,string) result) ->
                    string ->
                    ('a,'b err) Lwt_result.t
  val post_frame  : ?contents:t -> string -> response Lwt_xmlHttpRequest.generic_http_frame Lwt.t
  val post        : ?contents:t -> string -> (t,int) Lwt_result.t
  val post_result : ?contents:t ->
                    ?from_err:(t -> ('b,string) result) ->
                    (t -> ('a,string) result) ->
                    string ->
                    ('a,'b err) Lwt_result.t

  module WS : WS with type t := t

end

module Make(M:Req) : (Request with type t = M.t and type response = M.response) = struct

  type t        = M.t
  type response = M.response

  let get_frame addr : M.response Lwt_xmlHttpRequest.generic_http_frame Lwt.t =
    Lwt_xmlHttpRequest.perform_raw
      ~response_type:M.response_type
      addr

  let get addr : (t,int) Lwt_result.t =
    get_frame addr
    >|= fun frame ->
    if frame.code = 200
    then Ok (M.parse frame.content)
    else Error frame.code

  let get_result ?(from_err:(t -> ('b,string) result) option)
                 (from:t -> ('a,string) result)
                 addr : ('a,'b err) Lwt_result.t =
    get_frame addr
    >|= fun frame ->
    if frame.code = 200
    then match (from @@ M.parse frame.content) with
         | Ok _ as v -> v
         | Error _   -> Error (`Code frame.code)
    else match from_err with
         | Some f -> (match (f @@ M.parse frame.content) with
                      | Ok err  -> Error (`Data (frame.code,err))
                      | Error _ -> Error (`Code frame.code))
         | None   -> Error (`Code frame.code)

  let post_frame ?contents addr : response Lwt_xmlHttpRequest.generic_http_frame Lwt.t =
    let contents = match contents with
      | None   -> None
      | Some x -> Some (M.to_contents x)
    in
    Lwt_xmlHttpRequest.perform_raw
      ~content_type:M.content_type
      ~headers:[ ("Accept", M.accept); ("X-Requested-With", "XMLHttpRequest") ]
      ~override_method:`POST
      ?contents
      ~response_type:M.response_type
      addr

  let post ?contents addr : (t,int) Lwt_result.t =
    post_frame ?contents addr
    >|= fun frame ->
    if frame.code = 200
    then Ok (M.parse frame.content)
    else Error frame.code

  let post_result ?contents
                  ?(from_err:(t -> ('b,string) result) option)
                  (from:t -> ('a,string) result)
                  addr : ('a,'b err) Lwt_result.t =
    post_frame ?contents addr
    >|= fun frame ->
    if frame.code = 200
    then match (from @@ M.parse frame.content) with
         | Ok _ as v -> v
         | Error _   -> Error (`Code frame.code)
    else match from_err with
         | Some f -> (match (f @@ M.parse frame.content) with
                      | Ok err  -> Error (`Data (frame.code,err))
                      | Error _ -> Error (`Code frame.code))
         | None   -> Error (`Code frame.code)

  module WS = struct

    let create ?(port:int option) addr =
      let port = match port with Some p -> p | None -> 8080 in
      let addr = Js.string @@ Printf.sprintf
                                "ws://%s:%d/%s"
                                (Js.to_string Dom_html.window##.location##.hostname)
                                port
                                addr
      in
      new%js WebSockets.webSocket addr

    let get ?(port:int option) addr (from:t -> ('a,string) result) =
      let sock = create ?port addr in
      let ev, push = React.E.create () in
      sock##.onmessage := Dom.handler (fun (msg : WebSockets.webSocket WebSockets.messageEvent Js.t)
                                       -> M.of_socket_msg msg
                                          |> from
                                          |> (function Ok msg -> push msg | Error _ -> ());
                                          Js.bool true);
      ev,sock

  end

end

module Json_req : (Req with type t = json and type response = Js.js_string Js.t) = struct

  type t        = json
  type response = Js.js_string Js.t

  let content_type    = "application/json; charset=UTF-8"
  let accept          = "application/json, text/javascript, */*; q=0.01"
  let response_type   = XmlHttpRequest.Text
  let parse x         = Yojson.Safe.from_string @@ unwrap @@ Js.to_string x
  let to_contents x   = (`String (Yojson.Safe.to_string x) : 'a contents)
  let of_socket_msg m = Js.to_string m##.data |> Yojson.Safe.from_string

end

module Json_request : (Request with type t := json and type response := Js.js_string Js.t) = Make(Json_req)
