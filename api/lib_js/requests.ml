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

  val create : ?secure:bool ->
               ?host:string ->
               ?port:int ->
               ?path:string ->
               ?query:Uri.Query.t ->
               unit -> WebSockets.webSocket Js.t
  val get    : ?secure:bool ->
               ?host:string ->
               ?port:int ->
               ?path:string ->
               ?query:Uri.Query.t ->
               (t -> ('a,string) result) ->
               unit -> 'a React.event * WebSockets.webSocket Js.t

end

module type Request = sig

  type t
  type response

  val get_raw          : ?scheme:Uri.Scheme.t ->
                         ?host:string ->
                         ?port:int ->
                         ?path:string ->
                         ?query:Uri.Query.t ->
                         unit -> response Lwt_xmlHttpRequest.generic_http_frame Lwt.t
  val get              : ?scheme:Uri.Scheme.t ->
                         ?host:string ->
                         ?port:int ->
                         ?path:string ->
                         ?query:Uri.Query.t ->
                         unit -> (t,int) Lwt_result.t
  val get_result       : ?scheme:Uri.Scheme.t ->
                         ?host:string ->
                         ?port:int ->
                         ?path:string ->
                         ?query:Uri.Query.t ->
                         ?from_err:(t -> ('b,string) result) ->
                         (t -> ('a,string) result) ->
                         unit -> ('a,'b err) Lwt_result.t

  val post_raw         : ?scheme:Uri.Scheme.t ->
                         ?host:string ->
                         ?port:int ->
                         ?path:string ->
                         ?query:Uri.Query.t ->
                         ?contents:t ->
                         unit -> response Lwt_xmlHttpRequest.generic_http_frame Lwt.t
  val post             : ?scheme:Uri.Scheme.t ->
                         ?host:string ->
                         ?port:int ->
                         ?path:string ->
                         ?query:Uri.Query.t ->
                         ?contents:t ->
                         unit -> (t,int) Lwt_result.t
  val post_result      : ?scheme:Uri.Scheme.t ->
                         ?host:string ->
                         ?port:int ->
                         ?path:string ->
                         ?query:Uri.Query.t ->
                         ?contents:t ->
                         ?from_err:(t -> ('b,string) result) ->
                         (t -> ('a,string) result) ->
                         unit -> ('a,'b err) Lwt_result.t
  val post_result_unit : ?scheme:Uri.Scheme.t ->
                         ?host:string ->
                         ?port:int ->
                         ?path:string ->
                         ?query:Uri.Query.t ->
                         ?contents:t ->
                         ?from_err:(t -> ('b,string) result) ->
                         unit -> (unit,'b err) Lwt_result.t

  module WS : WS with type t := t

end

module Make(M:Req) : (Request with type t = M.t and type response = M.response) = struct

  type t        = M.t
  type response = M.response

  module Default = struct
    let host ()   = Js.to_string @@ Dom_html.window##.location##.hostname
    let path ()   = ""
    let scheme () = Js.to_string @@ Dom_html.window##.location##.protocol |> CCString.rdrop_while (Char.equal ':')
    let port ()   = Js.to_string @@ Dom_html.window##.location##.port |> int_of_string_opt
  end

  let make_uri ?(scheme=Default.scheme ())
               ?(host=Default.host ())
               ?port
               ?(path=Default.path ())
               ?query
               () =
    let port = match port with Some x -> Some x | None -> Default.port () in
    print_endline scheme;
    Uri.make ?port ?query ~scheme ~host ~path ()
    |> Uri.to_string
    |> Uri.pct_decode
    |> fun x -> print_endline x; x

  let get_raw ?scheme ?host ?port ?path ?query () =
    Lwt_xmlHttpRequest.perform_raw
      ~response_type:M.response_type
      (make_uri ?scheme ?host ?port ?path ?query ())

  let get ?scheme ?host ?port ?path ?query () =
    get_raw ?scheme ?host ?port ?path ?query ()
    >|= fun frame ->
    if frame.code = 200
    then Ok (M.parse frame.content)
    else Error frame.code

  let get_result ?scheme ?host ?port ?path ?query ?from_err from () =
    get_raw ?scheme ?host ?port ?path ?query ()
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

  let post_raw ?scheme ?host ?port ?path ?query ?contents () =
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
      (make_uri ?scheme ?host ?port ?path ?query ())

  let post ?scheme ?host ?port ?path ?query ?contents () =
    post_raw ?scheme ?host ?port ?path ?query ?contents ()
    >|= fun frame ->
    if frame.code = 200
    then Ok (M.parse frame.content)
    else Error frame.code

  let post_result ?scheme ?host ?port ?path ?query ?contents ?from_err from () =
    post_raw ?scheme ?host ?port ?path ?query ?contents ()
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

  let post_result_unit ?scheme ?host ?port ?path ?query ?contents ?from_err () =
    post_result ?scheme ?host ?port ?path ?query ?contents ?from_err (fun _ -> Ok ()) ()

  module WS = struct

    let scheme x = if x then Uri.Scheme.wss else Uri.Scheme.ws

    let create ?(secure=false) ?host ?port ?path ?query () =
      let scheme = scheme secure in
      let port = match port with Some x -> Some x | None -> Default.port () in
      new%js WebSockets.webSocket (Js.string @@ make_uri ?host ?port ?path ?query ~scheme ())

    let get ?secure ?host ?port ?path ?query from () =
      let sock = create ?secure ?host ?port ?path ?query () in
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
