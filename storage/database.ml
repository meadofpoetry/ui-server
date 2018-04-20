open Containers
open Lwt.Infix
open Common
   
module Settings = struct
  type t = { socket_path : string; cleanup : Time.Period.Hours.t; password : string } [@@deriving yojson]
  let default   = { socket_path = "/tmp"; cleanup = Time.Period.Hours.of_hours 1; password = "ats3" }
  let domain = "db"
end
                
module Conf = Config.Make(Settings)

let error s e =
  Printf.sprintf s (Caqti_error.show e)
            
let pool_use p f =
  Caqti_lwt.Pool.use (fun c -> f c >>= Lwt.return_ok) p >>= function
  | Error e -> Lwt.fail_with (Caqti_error.show e)
  | Ok    v -> Lwt.return v

let fail_if = function Ok v -> Lwt.return v | Error e -> Lwt.fail_with (Caqti_error.show e)

type state = { period  : float
             ; cleanup : Time.Period.Hours.t
             ; db      : ((module Caqti_lwt.CONNECTION), Caqti_error.connect) Caqti_lwt.Pool.t
             }

type simple
type trans
type (_,_,_) query =
  | Exec  : ('req, unit, [`Zero]) Caqti_request.t -> (simple, 'req, unit) query
  | Find  : ('req, 'resp, [`One | `Zero]) Caqti_request.t -> (simple, 'req, 'resp option) query
  | List  : ('req, 'resp, [`Many | `One | `Zero]) Caqti_request.t -> (simple, 'req, 'resp list) query
  | Trans : (simple,'req, 'resp) query * 'acc * ('acc -> 'resp -> 'acc) -> (trans, 'req list, 'acc) query

type db = { exec : 'typ 'request 'response. ('typ, 'request, 'response) query -> 'request -> 'response Lwt.t }
  
module type MODEL = sig
  type _ req
  val name     : string
  val table    : string
  val init     : db -> unit Lwt.t
  val request  : db -> 'a req -> 'a Lwt.t
  val worker   : (db -> unit Lwt.t) option
end
           
module type CONN = sig
  type t
  type _ req
  val create   : state -> (t, string) result
  val request  : t -> 'a req -> 'a Lwt.t
  val delete   : t -> unit Lwt.t
end

module Make (M : MODEL) : (CONN with type 'a req := 'a M.req) = struct
  type t = state

  let rec request : type typ req resp. (module Caqti_lwt.CONNECTION) -> (typ, req, resp) query -> req -> resp Lwt.t =
    fun (module Db : Caqti_lwt.CONNECTION) q args ->
    match q with
    | Exec q -> Db.exec q args >>= fail_if
    | Find q -> Db.find_opt q args >>= fail_if
    | List q -> Db.rev_collect_list q args >>= fail_if
    | Trans (q, acc, f) ->
       Db.start () >>= fail_if >>= fun () ->
       List.fold_left (fun acc arg ->
           acc >>= function
           | Error _ as e -> Lwt.return e
           | Ok v -> request (module Db : Caqti_lwt.CONNECTION) q arg >>= (fun r -> Lwt.return_ok (f v r)))
         (Lwt.return_ok acc) args
       >>= fail_if >>= (fun result -> Db.commit () >>= fail_if >>= fun () -> Lwt.return result)

  let wrap_req f db = f { exec = fun x -> request db x }
         
  let cleanup period (module Db : Caqti_lwt.CONNECTION) =
    let cleanup' =
      Caqti_request.exec Caqti_type.ptime_span
        (Printf.sprintf "DELETE FROM %s WHERE date <= (now()::TIMESTAMP - ?::INTERVAL)" M.table)
    in
    Db.exec cleanup' period >>= function
    | Ok ()   -> Lwt.return ()
    | Error e -> Lwt.fail_with (error "cleanup %s" e)
               
  let create (state : state) =
    let rec loop () =
      Lwt_unix.sleep state.period >>= (fun () ->
        match M.worker with
        | None   -> Lwt.return_unit
        | Some w -> pool_use state.db (wrap_req w)) >>= fun () ->
      pool_use state.db (cleanup state.cleanup) >>= loop
    in
    Lwt_main.run (pool_use state.db (wrap_req M.init));
    Lwt.async loop;
    Ok state

  let delete state =
    let delete' =
      Caqti_request.exec Caqti_type.unit
        (Printf.sprintf "DELETE FROM %s" M.table)
    in
    pool_use state.db (fun (module Db : Caqti_lwt.CONNECTION) -> Db.exec delete' ()) >>= function
    | Ok ()   -> Lwt.return ()
    | Error e -> Lwt.fail_with (error "delete %s" e) 

  let request (type a) state (req : a M.req) : a Lwt.t =
    pool_use state.db (fun c -> (wrap_req M.request c) req)
end

type t = state

let create config period =   
  let user = Sys.getenv "USER" in
  let settings = Conf.get config in
  let path = Printf.sprintf "postgresql://%s:%s/ats?host=/%s"
               user settings.password settings.socket_path in
  let db   = match Caqti_lwt.connect_pool ~max_size:50 (Uri.of_string path) with
    | Ok db   -> db
    | Error e -> failwith (Printf.sprintf "Db connect failed with an error: %s\n" @@ Caqti_error.show e)
  in
  { db; period; cleanup = settings.cleanup }
  
let finalize v =
  Lwt_main.run @@ Caqti_lwt.Pool.drain v.db
