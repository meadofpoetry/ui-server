open Js_of_ocaml_lwt
open Lwt.Infix
open Netlib

let ( % ) f g x = f (g x)

module type BODY = sig
  include Api.BODY
  val content_type : string
  val accept : string
end

type meth =
  [ `DELETE
  | `GET
  | `HEAD
  | `OPTIONS
  | `PATCH
  | `POST
  | `PUT
  ]

type env = string -> string option

type error =
  [ `Unauthorized
  | `Not_implemented
  | `Conv_error of string
  | `Error of string
  | `Timeout of float
  | `Unknown of int
  ]

let error_to_string : [< error] -> string = function
  | `Unauthorized -> "Unauthorized"
  | `Not_implemented -> "Not implemented"
  | `Conv_error s -> Printf.sprintf "Body parsing failed: %s" s
  | `Error s -> s
  | `Timeout _ -> Printf.sprintf "Request timed out"
  | `Unknown code -> Printf.sprintf "Unexpected response code: %d" code

let make_uri ?scheme ?host ?port ~f ~path ~query =
  let host = match host with
    | None -> Js_of_ocaml.Url.Current.host
    | Some x -> x in
  let port = match port with
    | None -> Js_of_ocaml.Url.Current.port
    | Some x -> Some x in
  Uri.kconstruct ?scheme ~host ?port
    ~f:(f % Uri.pct_decode % Uri.to_string) ~path ~query

let perform ?headers ?progress ?upload_progress ?contents ?content_type
    ?meth ?with_credentials ?scheme ?host ?port ~path ~query =
  let f uri cb : 'a Lwt.t =
    XmlHttpRequest.perform_raw_url
      ?headers
      ?progress
      ?upload_progress
      ?contents
      ?content_type
      ?override_method:meth
      ?with_credentials
      uri
    >>= fun (x : XmlHttpRequest.http_frame) ->
    let res = match Code.of_int x.code with
      | `Unauthorized -> Error `Unauthorized
      | `Not_implemented -> Error `Not_implemented
      | `Forbidden -> Error (`Error x.content)
      | `OK -> Ok x.content
      | _ -> Error (`Unknown x.code) in
    cb x.headers res in
  make_uri ?scheme ?host ?port ~f ~path ~query

let perform_file ?headers ?progress ?upload_progress
      ~file ?meth ?with_credentials ?scheme
      ?host ?port ~path ~query =
  let f uri cb : 'a Lwt.t =
    XmlHttpRequest.perform_raw_url
      ?headers 
      ?progress
      ?upload_progress
      ~content_type:"application/octet-stream"
      ~contents:(`Blob file)
      ?override_method:meth
      ?with_credentials
      uri
    >>= fun (x : XmlHttpRequest.http_frame) ->
    let res = match Code.of_int x.code with
      | `Unauthorized -> Error `Unauthorized
      | `Not_implemented -> Error `Not_implemented
      | `Forbidden -> Error (`Error x.content)
      | `OK -> Ok ()
      | _ -> Error (`Unknown x.code) in
    cb x.headers res in
  make_uri ?scheme ?host ?port ~f ~path ~query
                   
module Make(Body : BODY) : sig

  val perform : ?headers:(string * string) list
    -> ?progress:(int -> int -> unit)
    -> ?upload_progress:(int -> int -> unit)
    -> ?body:Body.t
    -> ?meth:meth
    -> ?with_credentials:bool
    -> ?scheme:string
    -> ?host:string
    -> ?port:int
    -> path:('b, 'c) Uri.Path.Format.t
    -> query:('c, (env -> (Body.t, error) result -> 'a Lwt.t) -> 'a Lwt.t) Uri.Query.format
    -> 'b

  val perform_unit : ?headers:(string * string) list
    -> ?progress:(int -> int -> unit)
    -> ?upload_progress:(int -> int -> unit)
    -> ?body:Body.t
    -> ?meth:meth
    -> ?with_credentials:bool
    -> ?scheme:string
    -> ?host:string
    -> ?port:int
    -> path:('b, 'c) Uri.Path.Format.t
    -> query:('c, (env -> (unit, error) result -> 'a Lwt.t) -> 'a Lwt.t) Uri.Query.format
    -> 'b

end = struct

  let perform ?headers ?progress ?upload_progress
      ?body ?meth ?with_credentials ?scheme
      ?host ?port ~path ~query =
    let content_type = match body with
      | None -> None
      | Some _ -> Some Body.content_type in
    let contents = match body with
      | None -> None
      | Some x -> Some (`String (Body.to_string x)) in
    let f uri cb : 'a Lwt.t =
      XmlHttpRequest.perform_raw_url
        ?headers
        ?progress
        ?upload_progress
        ?content_type
        ?contents
        ?override_method:meth
        ?with_credentials
        uri
      >>= fun (x : XmlHttpRequest.http_frame) ->
      let res = match Code.of_int x.code with
        | `Unauthorized -> Error `Unauthorized
        | `Not_implemented -> Error `Not_implemented
        | `Forbidden -> Error (`Error x.content)
        | `OK -> Body.of_string x.content
        | _ -> Error (`Unknown x.code) in
      cb x.headers res in
    make_uri ?scheme ?host ?port ~f ~path ~query

  let perform_unit ?headers ?progress ?upload_progress
      ?body ?meth ?with_credentials ?scheme
      ?host ?port ~path ~query =
    let content_type = match body with
      | None -> None
      | Some _ -> Some Body.content_type in
    let contents = match body with
      | None -> None
      | Some x -> Some (`String (Body.to_string x)) in
    let f uri cb : 'a Lwt.t =
      XmlHttpRequest.perform_raw_url
        ?headers
        ?progress
        ?upload_progress
        ?contents
        ?content_type
        ?override_method:meth
        ?with_credentials
        uri
      >>= fun (x : XmlHttpRequest.http_frame) ->
      let res = match Code.of_int x.code with
        | `Unauthorized -> Error `Unauthorized
        | `Not_implemented -> Error `Not_implemented
        | `Forbidden -> Error (`Error x.content)
        | `OK -> Ok ()
        | _ -> Error (`Unknown x.code) in
      cb x.headers res in
    make_uri ?scheme ?host ?port ~f ~path ~query

end
