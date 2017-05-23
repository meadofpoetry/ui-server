open Containers
open Lwt
open Lwt_react
open Cohttp_lwt_unix
open User
open Redirect

module type HANDLER = sig
  type state
  val queries : (Cohttp.Code.meth * string) list
  val handle  : state -> Cohttp.Code.meth * string -> string list -> 
                Cohttp.Header.t -> string ->
                (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
end

module type HANDLER_INST = sig
  module Handler : HANDLER
  val state : Handler.state
end

type key = Cohttp.Code.meth * string

module Handler_hash = struct
  type t = key
  let equal     = fun (m1, s1) (m2, s2) -> m1 = m2 && String.equal s1 s2
  let hash      = Hash.pair Hash.poly String.hash
  let to_string = fun (m,s) -> Cohttp.Code.string_of_method m ^ " " ^ s
end
   
module Handler_tbl = Hashtbl.Make(Handler_hash)

let build
      (type a)
      (module H : HANDLER with type state = a)
      state
  = (module struct
       module Handler = H
       let state = state
     end : HANDLER_INST)

let build_dispatch_table handlers =
  let open Result in  
  let rec add tbl keys value =
    match keys with
    | []      -> Ok ()
    | key::tl -> try let _ = Handler_tbl.find tbl key
                     in Error ("key already exists: " ^ Handler_hash.to_string key)
                 with Not_found ->
                   Handler_tbl.add tbl key value;
                   add tbl tl value
  in 
  let table = Handler_tbl.create 200 in
  List.fold_while
    (fun _ ((module I : HANDLER_INST) as instance) ->
      match add table I.Handler.queries instance with
      | Ok ()   as ok  -> (ok, `Continue)
      | Error _ as err -> (err, `Stop))
    (Ok ())
    handlers
  |> function
    | Ok ()          -> Ok table
    | Error _ as err -> err

let handle table meth path headers body =
  try
    let query = (meth, List.hd path) in
    let (module Inst : HANDLER_INST) = Handler_tbl.find table query in
    Inst.Handler.handle Inst.state query (List.tl path) headers body
  with
  | _ -> not_found ()

  (*
let handle ~database
           meth path headers body =
  let redir = redirect_auth database headers in
  match meth, path with
  | `POST, ["login"]  -> redir (fun _ -> home_page ()) (fun () -> Authorize.auth database headers)
  | `GET,  ["test"]   -> Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"test" ()
  | _ -> not_found ()
   *)
