open Lwt.Infix
open Common

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

let unwrap = Uri.pct_decode

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

  val create' :
    ?secure:bool ->
    ?host:string ->
    ?port:int ->
    f:(WebSockets.webSocket Js.t -> 'a) ->
    path:('b, 'c) Uri.Path.Format.t ->
    query:('c, 'a) Uri.Query.compose -> 'b
  val create :
    ?secure:bool ->
    ?host:string ->
    ?port:int ->
    path:('a, 'b) Uri.Path.Format.t ->
    query:('b, WebSockets.webSocket Js.t) Uri.Query.compose -> 'a
  val get :
    ?secure:bool ->
    ?host:string ->
    ?port:int ->
    from:(t -> ('c, string) result) ->
    path:('a, 'b) Uri.Path.Format.t ->
    query:('b, 'c React.event * WebSockets.webSocket Js.t)
          Uri.Query.compose -> 'a

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
    query:('f, ('c, [> `Code of int | `Data of int * 'a ]) result Lwt.t)
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
    query:('f, ('c, [> `Code of int | `Data of int * 'a ]) result Lwt.t)
          Uri.Query.compose ->
    'e
  val post_result_unit :
    ?scheme:string ->
    ?host:string ->
    ?port:int ->
    ?contents:t ->
    ?from_err:(t -> ('a, string) result) ->
    path:('c, 'd) Uri.Path.Format.t ->
    query:('d, (unit, [> `Code of int | `Data of int * 'a ]) result Lwt.t)
          Uri.Query.compose ->
    'c

  module WS : WS with type t := t

end

module Make(M:Req) : (Request with type t = M.t and type response = M.response) = struct

  type t        = M.t
  type response = M.response

  type frame    = response Lwt_xmlHttpRequest.generic_http_frame

  module Default = struct
    let host ()   = Js.to_string @@ Dom_html.window##.location##.hostname
    let path ()   = ""
    let scheme () = Js.to_string @@ Dom_html.window##.location##.protocol |> CCString.rdrop_while (Char.equal ':')
    let port ()   = Js.to_string @@ Dom_html.window##.location##.port |> int_of_string_opt
  end

  let make_uri ?(scheme=Default.scheme ()) ?(host=Default.host ()) ?port ~f ~path ~query =
    let port = match port with
      | None   -> Default.port ()
      | Some p -> Some p
    in
    Uri.kconstruct ~scheme ~host ?port ~f:(fun u -> Uri.to_string u |> Uri.pct_decode |> f) ~path ~query

  let get_raw' ?scheme ?host ?port ~f ~path ~query =
    let f = fun uri -> Lwt_xmlHttpRequest.perform_raw ~response_type:M.response_type uri >|= f in
    make_uri ?scheme ?host ?port ~f ~path ~query

  let get_raw ?scheme ?host ?port ~path ~query =
    get_raw' ?scheme ?host ?port ~f:(fun x -> x) ~path ~query

  let get ?scheme ?host ?port ~path ~query =
    let f = fun (frame:frame) ->
      if frame.code = 200
      then Ok (M.parse frame.content)
      else Error frame.code
    in get_raw' ?scheme ?host ?port ~f ~path ~query

  let get_result ?scheme ?host ?port ?from_err ~from ~path ~query =
    let f = fun (frame:frame) ->
      if frame.code = 200
      then match (from @@ M.parse frame.content) with
           | Ok _ as v -> v
           | Error _   -> Error (`Code frame.code)
      else match from_err with
           | Some f -> (match (f @@ M.parse frame.content) with
                        | Ok err  -> Error (`Data (frame.code,err))
                        | Error _ -> Error (`Code frame.code))
           | None   -> Error (`Code frame.code)
    in get_raw' ?scheme ?host ?port ~f ~path ~query

  let post_raw' ?scheme ?host ?port ?contents ~f ~path ~query =
    let contents = match contents with
      | None   -> None
      | Some x -> Some (M.to_contents x)
    in
    let f = fun uri ->
      Lwt_xmlHttpRequest.perform_raw
        ~content_type:M.content_type
        ~headers:[ ("Accept", M.accept); ("X-Requested-With", "XMLHttpRequest") ]
        ~override_method:`POST
        ?contents
        ~response_type:M.response_type
        uri
      >|= f
    in make_uri ?scheme ?host ?port ~f ~path ~query

  let post_raw ?scheme ?host ?port ?contents ~path ~query =
    post_raw' ?scheme ?host ?port ?contents ~f:(fun x -> x) ~path ~query

  let post ?scheme ?host ?port ?contents ~path ~query =
    let f = fun (frame:frame) ->
      if frame.code = 200
      then Ok (M.parse frame.content)
      else Error frame.code
    in post_raw' ?scheme ?host ?port ?contents ~f ~path ~query

  let post_result ?scheme ?host ?port ?contents ?from_err ~from ~path ~query =
    let f = fun (frame:frame) ->
      if frame.code = 200
      then match (from @@ M.parse frame.content) with
           | Ok _ as v -> v
           | Error _   -> Error (`Code frame.code)
      else match from_err with
           | Some f -> (match (f @@ M.parse frame.content) with
                        | Ok err  -> Error (`Data (frame.code,err))
                        | Error _ -> Error (`Code frame.code))
           | None   -> Error (`Code frame.code)
    in post_raw' ?scheme ?host ?port ?contents ~f ~path ~query

  let post_result_unit ?scheme ?host ?port ?contents ?from_err ~path ~query =
    post_result ?scheme ?host ?port ?contents ?from_err ~from:(fun _ -> Ok ()) ~path ~query

  module WS = struct

    let scheme secure = if secure then Uri.Scheme.wss else Uri.Scheme.ws

    let create' ?(secure=false) ?host ?port ~f ~path ~query =
      let scheme = scheme secure in
      let f = fun uri ->
        new%js WebSockets.webSocket (Js.string uri)
        |> f
      in make_uri ~scheme ?host ?port ~f ~path ~query

    let create ?secure ?host ?port ~path ~query =
      create' ?secure ?host ?port ~f:(fun x -> x) ~path ~query

    let get ?secure ?host ?port ~from ~path ~query =
      let f = fun sock ->
        let ev, push = React.E.create () in
        sock##.onmessage := Dom.handler (fun (msg : WebSockets.webSocket WebSockets.messageEvent Js.t)
                                         -> M.of_socket_msg msg
                                            |> from
                                            |> (function Ok msg -> push msg | Error _ -> ());
                                            Js.bool true);
        ev,sock
      in create' ?secure ?host ?port ~f ~path ~query

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
