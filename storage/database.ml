open Containers
open Lwt.Infix
open Common
   
module Settings = struct
  type t = { socket_path : string; cleanup : Time.Period.Hours.t; password : string } [@@deriving yojson]
  let default   = { socket_path = "/tmp"; cleanup = Time.Period.Hours.of_int 1; password = "ats3" }
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
  | Exec     : ('req, unit, [`Zero]) Caqti_request.t -> (simple, 'req, unit) query
  | Find     : ('req, 'resp, [`One | `Zero]) Caqti_request.t -> (simple, 'req, 'resp option) query
  | List     : ('req, 'resp, [`Many | `One | `Zero]) Caqti_request.t -> (simple, 'req, 'resp list) query
  (* | Map *)
  | Reduce   : (simple, 'req, 'resp) query * 'acc * ('acc -> 'resp -> 'acc) -> (trans, 'req list, 'acc) query
  | Maintain : (simple, 'req, unit) query list -> (trans, 'req, unit) query

type db = { exec : 'typ 'request 'response. ('typ, 'request, 'response) query -> 'request -> 'response Lwt.t }

module Key_t : sig
  type t
  val key : ?default:string -> string -> t
  val to_string : t -> string
end = struct
  type t = string
  let key ?default (typ : string) : t = match default with
    | None -> typ
    | Some def -> typ ^ " DEFAULT " ^ def (* TODO add check *)
  let to_string (t : t) : string = t
end
                
type keys = { columns  : (string * Key_t.t) list
            ; time_key : string option
            }
        
module type MODEL = sig
  val name     : string
  val tables   : (string * keys * (simple,unit,unit) query option) list
end
           
module type CONN = sig
  type t
  val create   : state -> (t, string) result
  val request  : t -> ('typ, 'req, 'resp) query -> 'req -> 'resp Lwt.t
  val delete   : t -> unit Lwt.t
end

module Make (M : MODEL) : CONN = struct
  type t = state

  let make_init_query name keys =
    let cols = String.concat ", " (List.map (fun (k,t) -> k ^ " " ^ (Key_t.to_string t)) keys) in
    let exp = Printf.sprintf "CREATE TABLE IF NOT EXISTS %s (%s)" name cols in
    Exec (Caqti_request.exec Caqti_type.unit exp)

  let init_trans = Maintain (List.map (fun (name,keys,_) -> make_init_query name keys.columns) M.tables)

  let workers_trans =
    List.filter_map (fun (_,_,w) -> w) M.tables
    |> function [] -> None | lst -> Some (Maintain lst)           
                 
  let cleanup_trans =
    Maintain (List.filter_map (fun (table,keys,_) ->
                  match keys.time_key with
                  | None -> None
                  | Some time_key ->
                     Some (Exec (Caqti_request.exec Caqti_type.ptime_span
                                   (Printf.sprintf "DELETE FROM %s WHERE %s <= (now()::TIMESTAMP - ?::INTERVAL)"
                                      table time_key))))
                M.tables)

  let delete_trans =
    Maintain (List.map (fun (table,_,_) ->
                  Exec (Caqti_request.exec Caqti_type.unit
                          (Printf.sprintf "DELETE FROM %s" table)))
                M.tables)

  let rec request : type typ req resp. (module Caqti_lwt.CONNECTION) -> (typ, req, resp) query -> req -> (resp, [< Caqti_error.t]) result Lwt.t =
    fun (module Db : Caqti_lwt.CONNECTION) q args ->
    match q with
    | Exec q -> Db.exec q args
    | Find q -> Db.find_opt q args
    | List q -> Db.rev_collect_list q args
    | Reduce (q, acc, f) ->
       Db.start () >>= fail_if >>= fun () ->
       List.fold_left (fun acc arg ->
           let open Lwt_result in
           acc
           >>= fun v -> request (module Db : Caqti_lwt.CONNECTION) q arg
           >>= fun r -> Lwt.return_ok (f v r))
         (Lwt.return_ok acc) args
       >>= (fun result -> Db.commit () >>= fail_if >>= fun () -> Lwt.return result)
    | Maintain ql ->
       Db.start () >>= fail_if >>= fun () ->
       List.fold_left (fun acc q ->
           let open Lwt_result in
           acc >>= fun () -> request (module Db : Caqti_lwt.CONNECTION) q args)
         (Lwt.return_ok ()) ql
       >>= (fun result -> Db.commit () >>= fail_if >>= fun () -> Lwt.return result)

  let wrap_query : type req resp. ('a, req, resp) query -> req -> (module Caqti_lwt.CONNECTION) -> resp Lwt.t =
    fun query args db -> request db query args >>= fail_if
               
  let create (state : state) =
    let rec loop () =
      Lwt_unix.sleep state.period >>= (fun () ->
        match workers_trans with
        | None   -> Lwt.return_unit
        | Some w -> pool_use state.db (wrap_query w ()))
      >>= fun () ->
      pool_use state.db (wrap_query cleanup_trans state.cleanup) >>= loop
    in
    Lwt_main.run (pool_use state.db (wrap_query init_trans ()));
    Lwt.async loop;
    Ok state

  let delete state =
    pool_use state.db (wrap_query delete_trans ())

    (*>>= function
    | Ok ()   -> Lwt.return ()
    | Error e -> Lwt.fail_with (error "delete %s" e) *)

  let request (type req resp) state (q : ('a, req, resp) query) (args : req) : resp Lwt.t =
    pool_use state.db (wrap_query q args)
end

module Types = struct
  include Caqti_type

  let (&) = tup2

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
