type env = Api.env

type 'a response = 'a Api.response
(*
let is_ws headers = match Cohttp.Header.get headers "upgrade" with
  | Some "websocket" -> true
  | _                -> false
 *)

let env_of_headers headers : env =
  let env : type a. a Api.key -> a option = function
    | Api.Key k ->
       Cohttp.Header.get headers k
    | Api.Auth ->
       match Cohttp.Header.get_authorization headers with
       | Some (`Basic b) -> Some b
       | _ -> None
  in { env }

let respond_need_auth = Cohttp_lwt_unix.Server.respond_need_auth

(*
let respond_not_found = Cohttp_lwt_unix.Server.respond_not_found

let respond_file base path =
  Cohttp_lwt_unix.Server.respond_file
    ~fname:(Filename.concat base path)
 *)
let respond_string ?(status = `OK) body =
  Cohttp_lwt_unix.Server.respond_string ~status ~body
(*
let respond_html_elt ?(status = `OK) body =
  Cohttp_lwt_unix.Server.respond ~status
    ~body:(Cohttp_lwt.Body.of_string
           @@ Format.asprintf "%a" (Tyxml.Html.pp_elt ()) body)

let respond_redirect path =
  Cohttp_lwt_unix.Server.respond_redirect
    ~uri:(Uri.with_path Uri.empty path)
 *)
let respond_error ?(status = `Forbidden) error =
  Cohttp_lwt_unix.Server.respond_error ~status ~body:error

(* TODO make use of Result resp types
  
let respond' ?headers ?flush ?(status = `OK) ~body () =
  let headers = match M.content_type with
    | Some s -> Some (Cohttp.Header.add_opt_unless_exists headers "Content-Type" s)
    | None -> None
  in Cohttp_lwt_unix.Server.respond ?headers ?flush ~status ~body ()

let respond ?headers ?flush ?status x () =
  respond' ?headers ?flush ?status ~body:(to_body x) ()

let respond_result ?(err_status=`Bad_request) = function
  | Ok x -> respond x ()
  | Error x -> respond ~status:err_status x ()

let respond_result_unit ?(err_status=`Bad_request) = function
  | Ok () -> respond' ~body:Cohttp_lwt.Body.empty ()
  | Error x -> respond ~status:err_status x ()

 *)

module Redirect = Redirect
                
module Make (User : Api.USER) (Body : Api.BODY) : sig

  include Api.S
          with type state = Cohttp_lwt_unix.Request.t * Conduit_lwt_unix.flow
           and type user = User.t
           and type body = Body.t
           and type meth = Cohttp.Code.meth
           and type path = Netlib.Uri.t
           and type answer = [ Api.Authorize.error
                             | Body.t response
                             | `Instant of (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
                             ]
           and type response = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
           and type 'a handler =
                      Cohttp.Code.meth * 'a Netlib.Uri.Dispatcher.node

  val make : domain:string
             -> node list
             -> t

  val node : ?doc:string
             -> ?restrict:user list
             -> meth:meth
             -> path:('a, 'b) Netlib.Uri.Path.Format.t
             -> query:('b, user -> body -> env -> state -> answer)
                  Netlib.Uri.Query.format
             -> 'a
             -> node

end = struct

  open Netlib

  module Meth_map = Map.Make (struct
                        type t = Cohttp.Code.meth
                        let compare : t -> t -> int = Pervasives.compare
                      end) 

  type state = Cohttp_lwt_unix.Request.t * Conduit_lwt_unix.flow

  type user = User.t

  type body = Body.t

  type meth = Cohttp.Code.meth

  type path = Netlib.Uri.t

  type answer = [ Api.Authorize.error
                | Body.t response
                | `Instant of (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
                ] 

  type response = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

  type 'a handler =
    Cohttp.Code.meth * 'a Netlib.Uri.Dispatcher.node

  type node =
    (user -> body -> env -> state -> answer) handler

  type t =
    (user -> body -> env -> state -> answer)
      Netlib.Uri.Dispatcher.t
      Meth_map.t 

  let handle tbl ~state ?(meth=`GET) ~env ~redir uri body =
    let body = match Body.of_string body with
      | Ok v -> v
      | Error _ -> failwith ""
    in
    let default (user : user) body env state =
      ignore (user, body, env, state); 
      `Instant (respond_error "bad request" ())
    in
    redir @@ (fun user ->
      let ans = match Meth_map.find_opt meth tbl with
        | None ->
           default user body env state
        | Some tbl ->
           Uri.Dispatcher.dispatch ~default tbl uri user body env state
      in match ans with (* TODO check resp types *)
         | #Api.Authorize.error ->
            respond_need_auth ~auth:(`Basic "User Visible Realm") ()
         | `Instant resp ->
            resp
         | `Value body ->
            respond_string (Body.to_string body) ()
         | `Unit ->
            respond_string "" () (* TODO there should be something better that string *)
         | `Error e ->
            respond_error e () ) 

  let make ~domain nodes =
    let path = Uri.Path.of_string domain in
    let add_node map (meth, node) =
      let update = function
        | None ->
           Some Uri.Dispatcher.(add empty (prepend path node))
        | Some disp ->
           Some Uri.Dispatcher.(add disp (prepend path node))
      in Meth_map.update meth update map
    in 
    nodes
    |> List.fold_left add_node Meth_map.empty

  let merge ~domain handlers =
    let path = Uri.Path.of_string domain in
    let flat _meth l r =
      match l, r with
      | Some tl, Some h -> Some (h::tl)
      | (Some _ as v), None -> v
      | None, Some h -> Some [h]
      | None, None -> None
    in
    handlers 
    |> List.fold_left (Meth_map.merge flat) Meth_map.empty
    |> Meth_map.map (fun met ->
           Uri.Dispatcher.(merge empty (List.map (fun x -> path, x) met)))

  let transform not_allowed f =
    fun user body env state ->
    if not_allowed user
    then `Error "access denied"
    else f user body env state
    
  let node ?doc ?(restrict=[]) ~meth ~path ~query handler : node =
    let not_allowed id = List.exists (User.equal id) restrict in
    Uri.Dispatcher.make ?docstring:doc ~path ~query handler
    |> Uri.Dispatcher.map_node (transform not_allowed)
    |> fun node -> meth, node

end

