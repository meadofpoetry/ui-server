open Js_of_ocaml
open Lwt.Infix
open Common

let code_to_string : int -> string = function
  | 100 -> "Continue"
  | 101 -> "Switching Protocols"
  | 200 -> "OK"
  | 201 -> "Created"
  | 202 -> "Accepted"
  | 203 -> "Non-Authoritative Information"
  | 204 -> "No Content"
  | 205 -> "Reset Content"
  | 206 -> "Partial Content"
  | 300 -> "Multiple Choices"
  | 301 -> "Moved Permanently"
  | 302 -> "Found"
  | 303 -> "See Other"
  | 304 -> "Not Modified"
  | 305 -> "Use Proxy"
  | 307 -> "Temporary Redirect"
  | 400 -> "Bad Request"
  | 401 -> "Unauthorized"
  | 402 -> "Payment Required"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 406 -> "Not Acceptable"
  | 407 -> "Proxy Authentication Required"
  | 408 -> "Request Time-out"
  | 409 -> "Conflict"
  | 410 -> "Gone"
  | 411 -> "Length Required"
  | 412 -> "Precondition Failed"
  | 413 -> "Request Entity Too Large"
  | 414 -> "Request-URI Too Large"
  | 415 -> "Unsupported Media Type"
  | 416 -> "Requested range not satisfiable"
  | 417 -> "Expectation Failed"
  | 500 -> "Internal Server Error"
  | 501 -> "Not Implemented"
  | 502 -> "Bad Gateway"
  | 503 -> "Service Unavailable"
  | 504 -> "Gateway Time-out"
  | 505 -> "HTTP Version not supported"
  | x -> Printf.sprintf "Unknown HTTP status code: %d" x


type 'a err =
  { code : int
  ; error : string option
  ; data : 'a option
  }

type json = Yojson.Safe.json

type 'a contents =
  [ `Blob of (#File.blob Js.t as 'a)
  | `Form_contents of Form.form_contents
  | `POST_form of (string * Form.form_elt) list
  | `String of string
  ]

let err_to_string : 'a. ?to_string:('a -> string) -> 'a err -> string =
  fun ?to_string err ->
  let base = Printf.sprintf "%d %s" err.code (code_to_string err.code) in
  match err.data with
  | Some a ->
     begin match to_string with
     | None -> base
     | Some f -> base ^ Printf.sprintf ".\n%s" (f a)
     end
  | None ->
     begin match err.error with
     | None -> base
     | Some e -> base ^ Printf.sprintf ".\n%s" e
     end

module type Req = sig
  open WebSockets

  type t
  type response

  val content_type : string
  val accept : string
  val response_type : response XmlHttpRequest.response
  val to_error_message : response -> string option
  val parse : response -> t
  val to_contents : t -> 'a contents
  val of_socket_msg : webSocket messageEvent Js.t -> t

end

module type WS = sig
  open WebSockets

  type t

  val create' :
    ?secure:bool ->
    ?host:string ->
    ?port:int ->
    f:(webSocket Js.t -> 'a) ->
    path:('b, 'c) Uri.Path.Format.t ->
    query:('c, 'a) Uri.Query.compose -> 'b
  val create :
    ?secure:bool ->
    ?host:string ->
    ?port:int ->
    path:('a, 'b) Uri.Path.Format.t ->
    query:('b, webSocket Js.t) Uri.Query.compose -> 'a
  val get :
    ?secure:bool ->
    ?host:string ->
    ?port:int ->
    from:(t -> ('c, string) result) ->
    path:('a, 'b) Uri.Path.Format.t ->
    query:('b, 'c React.event * webSocket Js.t)  Uri.Query.compose -> 'a

end

module type Request = sig

  type t
  type response

  val get_raw' :
    ?scheme:string ->
    ?host:string ->
    ?port:int ->
    f:(response Lwt_xmlHttpRequest.generic_http_frame -> 'a) ->
    path:('b, 'c) Uri.Path.Format.t ->
    query:('c, 'a Lwt.t) Uri.Query.compose -> 'b
  val get_raw :
    ?scheme:string ->
    ?host:string ->
    ?port:int ->
    path:('a, 'b) Uri.Path.Format.t ->
    query:('b, response Lwt_xmlHttpRequest.generic_http_frame Lwt.t)
          Uri.Query.compose ->
    'a
  val get :
    ?scheme:string ->
    ?host:string ->
    ?port:int ->
    path:('a, 'b) Uri.Path.Format.t ->
    query:('b, (t, int) result Lwt.t) Uri.Query.compose -> 'a
  val get_result :
    ?scheme:string ->
    ?host:string ->
    ?port:int ->
    ?from_err:(t -> ('a, string) result) ->
    from:(t -> ('c, string) result) ->
    path:('e, 'f) Uri.Path.Format.t ->
    query:('f, ('c, 'a err) result Lwt.t)
          Uri.Query.compose ->
    'e
  val post_raw' :
    ?scheme:string ->
    ?host:string ->
    ?port:int ->
    ?contents:t ->
    f:(response Lwt_xmlHttpRequest.generic_http_frame -> 'a) ->
    path:('b, 'c) Uri.Path.Format.t ->
    query:('c, 'a Lwt.t) Uri.Query.compose -> 'b
  val post_raw :
    ?scheme:string ->
    ?host:string ->
    ?port:int ->
    ?contents:t ->
    path:('a, 'b) Uri.Path.Format.t ->
    query:('b, response Lwt_xmlHttpRequest.generic_http_frame Lwt.t)
          Uri.Query.compose ->
    'a
  val post :
    ?scheme:string ->
    ?host:string ->
    ?port:int ->
    ?contents:t ->
    path:('a, 'b) Uri.Path.Format.t ->
    query:('b, (t, int) result Lwt.t) Uri.Query.compose -> 'a
  val post_result :
    ?scheme:string ->
    ?host:string ->
    ?port:int ->
    ?contents:t ->
    ?from_err:(t -> ('a, string) result) ->
    from:(t -> ('c, string) result) ->
    path:('e, 'f) Uri.Path.Format.t ->
    query:('f, ('c, 'a err) result Lwt.t)
          Uri.Query.compose ->
    'e
  val post_result_unit :
    ?scheme:string ->
    ?host:string ->
    ?port:int ->
    ?contents:t ->
    ?from_err:(t -> ('a, string) result) ->
    path:('c, 'd) Uri.Path.Format.t ->
    query:('d, (unit, 'a err) result Lwt.t)
          Uri.Query.compose ->
    'c

  module WS : WS with type t := t

end

module Make(M : Req) : (Request with type t = M.t and type response = M.response) = struct

  type t = M.t
  type response = M.response

  type frame = response Lwt_xmlHttpRequest.generic_http_frame

  module Default = struct
    let host () : string =
      Js.to_string @@ Dom_html.window##.location##.hostname

    let path () : string = ""

    let scheme () : string =
      Js.to_string @@ Dom_html.window##.location##.protocol
      |> CCString.rdrop_while (Char.equal ':')

    let port () : int option =
      Js.to_string @@ Dom_html.window##.location##.port
      |> int_of_string_opt
  end

  let make_uri ?(scheme = Default.scheme ()) ?(host = Default.host ())
        ?port ~f ~path ~query =
    let port = match port with
      | None -> Default.port ()
      | Some p -> Some p in
    Uri.kconstruct ~scheme ~host ?port
      ~f:CCFun.(f % Uri.pct_decode % Uri.to_string) ~path ~query

  let to_err ?data ?error (frame : frame) =
    { code = frame.code
    ; error = CCOpt.choice [error; M.to_error_message frame.content]
    ; data
    }

  let get_raw' ?scheme ?host ?port ~f ~path ~query =
    let f uri = M.(Lwt_xmlHttpRequest.perform_raw ~response_type uri) >|= f in
    make_uri ?scheme ?host ?port ~f ~path ~query

  let get_raw ?scheme ?host ?port ~path ~query =
    get_raw' ?scheme ?host ?port ~f:(fun x -> x) ~path ~query

  let get ?scheme ?host ?port ~path ~query =
    let f = fun (frame : frame) ->
      if frame.code = 200
      then Ok (M.parse frame.content)
      else Error frame.code
    in
    get_raw' ?scheme ?host ?port ~f ~path ~query

  let get_result ?scheme ?host ?port ?from_err ~from ~path ~query =
    let f = fun (frame : frame) ->
      if frame.code = 200
      then match from @@ M.parse frame.content with
           | Ok _ as v -> v
           | Error s -> Error (to_err ~error:s frame)
      else match from_err with
           | None -> Error (to_err frame)
           | Some f ->
              begin match f @@ M.parse frame.content with
              | Ok data -> Error (to_err ~data frame)
              | Error _ -> Error (to_err frame)
              end
    in
    get_raw' ?scheme ?host ?port ~f ~path ~query

  let post_raw' ?scheme ?host ?port ?contents ~f ~path ~query =
    let contents = match contents with
      | None -> None
      | Some x -> Some (M.to_contents x) in
    let f = fun uri ->
      M.(Lwt_xmlHttpRequest.perform_raw
           ~content_type
           ~headers:[("Accept", accept); ("X-Requested-With", "XMLHttpRequest") ]
           ~override_method:`POST
           ?contents
           ~response_type
           uri)
      >|= f
    in
    make_uri ?scheme ?host ?port ~f ~path ~query

  let post_raw ?scheme ?host ?port ?contents ~path ~query =
    post_raw' ?scheme ?host ?port ?contents ~f:(fun x -> x) ~path ~query

  let post ?scheme ?host ?port ?contents ~path ~query =
    let f = fun (frame : frame) ->
      if frame.code <> 200 then Error frame.code
      else Ok (M.parse frame.content)
    in
    post_raw' ?scheme ?host ?port ?contents ~f ~path ~query

  let post_result ?scheme ?host ?port ?contents ?from_err ~from ~path ~query =
    let f = fun (frame : frame) ->
      if frame.code = 200
      then match from @@ M.parse frame.content with
           | Ok _ as v -> v
           | Error s -> Error (to_err ~error:s frame)
      else match from_err with
           | None -> Error (to_err frame)
           | Some f ->
              begin match f @@ M.parse frame.content with
              | Ok data -> Error (to_err ~data frame)
              | Error _ -> Error (to_err frame)
              end
    in
    post_raw' ?scheme ?host ?port ?contents ~f ~path ~query

  let post_result_unit ?scheme ?host ?port ?contents ?from_err ~path ~query =
    post_result ?scheme ?host ?port ?contents ?from_err
      ~from:(fun _ -> Ok ()) ~path ~query

  module WS = struct

    let scheme (secure : bool) : string =
      Uri.Scheme.(if secure then wss else ws)

    let create' ?(secure = false) ?host ?port ~f ~path ~query =
      let scheme = scheme secure in
      let f uri = f @@ new%js WebSockets.webSocket (Js.string uri) in
      make_uri ~scheme ?host ?port ~f ~path ~query

    let create ?secure ?host ?port ~path ~query =
      create' ?secure ?host ?port ~f:(fun x -> x) ~path ~query

    let get ?secure ?host ?port ~from ~path ~query =
      let f = fun sock ->
        let open WebSockets in
        let ev, push = React.E.create () in
        let handler = fun (msg : webSocket messageEvent Js.t) ->
          M.of_socket_msg msg
          |> from
          |> (function Ok msg -> push msg | Error _ -> ());
          Js._true in
        sock##.onmessage := Dom.handler handler;
        ev, sock
      in
      create' ?secure ?host ?port ~f ~path ~query

  end

end

module Json_req : (Req with type t = json and type response = Js.js_string Js.t) = struct

  type t = json
  type response = Js.js_string Js.t

  let content_type = "application/json; charset=UTF-8"
  let accept = "application/json, text/javascript, */*; q=0.01"
  let response_type = XmlHttpRequest.Text

  let parse x =
    let s = Uri.pct_decode @@ Js.to_string x in
    (try Yojson.Safe.from_string s
     with _ -> `String s)
  let to_error_message = fun s ->
    let s = match parse s with
      | `String s -> s
      | _ -> Js.to_string s in
    if CCString.is_empty s then None else Some s
  let to_contents x = (`String (Yojson.Safe.to_string x) : 'a contents)
  let of_socket_msg m = Js.to_string m##.data |> Yojson.Safe.from_string

end

module Json_request : (Request with type t := json
                                and type response := Js.js_string Js.t) = Make(Json_req)
