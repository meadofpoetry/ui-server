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

module Request : sig
  type ('a, 'typ) t
  val (>>=) : ('a, 'typ) t -> ('a -> ('b, 'typ) t) -> ('b, 'typ) t
  val return : 'a -> ('a, [> `Simple]) t
  val exec : ('a, unit, [`Zero]) Caqti_request.t -> 'a -> (unit, [> `Simple]) t
  val find : ('a, 'b, [`One | `Zero]) Caqti_request.t -> 'a -> ('b option, [> `Simple]) t
  val list : ('a, 'b, [`Many | `One | `Zero]) Caqti_request.t -> 'a -> ('b list, [> `Simple]) t
  val with_trans : ('a, [`Simple]) t -> ('a, [> `Trans]) t
  val run : (module Caqti_lwt.CONNECTION) -> ('a, 'typ) t -> 'a Lwt.t
end = struct
  type ('a,_) t = (module Caqti_lwt.CONNECTION) -> 'a Lwt.t

  let return x = fun _ -> Lwt.return x

  let (>>=) m f =
    fun db -> let r = m db in Lwt.(r >>= fun x -> f x db)

  let exec q arg = fun (module Db: Caqti_lwt.CONNECTION) -> Lwt.(Db.exec q arg >>= fail_if)

  let find q arg = fun (module Db: Caqti_lwt.CONNECTION) -> Lwt.(Db.find_opt q arg >>= fail_if)

  let list q arg = fun (module Db: Caqti_lwt.CONNECTION) -> Lwt.(Db.rev_collect_list q arg >>= fail_if)

  let with_trans m = fun (module Db: Caqti_lwt.CONNECTION) ->
    let open Lwt.Infix in
    Db.start () >>= fail_if >>= fun () ->
    m (module Db: Caqti_lwt.CONNECTION) >>= fun res ->
    Db.commit () >>= fail_if >>= fun () -> Lwt.return res
                                                          
  let run db m = m db

end

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
  val tables   : (string * keys * (unit, _) Request.t option) list
end
           
module type CONN = sig
  type t
  val create   : state -> (t, string) result
  val request  : t -> ('req,_) Request.t -> 'req Lwt.t
  val delete   : t -> unit Lwt.t
end

module Make (M : MODEL) : CONN = struct
  
  type t = state

  let make_init_query name keys =
    let cols = String.concat ", " (List.map (fun (k,t) -> k ^ " " ^ (Key_t.to_string t)) keys) in
    let exp = Printf.sprintf "CREATE TABLE IF NOT EXISTS %s (%s)" name cols in
    Request.exec (Caqti_request.exec Caqti_type.unit exp)
 
  let init_trans =
    let open Request in
    with_trans (List.fold_left (fun acc (name,keys,_) -> acc >>= make_init_query name keys.columns)
                  (return ()) M.tables)

  let workers_trans =
    let open Request in
    List.filter_map (fun (_,_,w) -> w) M.tables
    |> function [] -> None
              | lst -> Some (with_trans (List.fold_left (fun acc m -> acc >>= fun () -> m) (return ()) lst))
                 
  let cleanup_trans cleanup_dur =
    let open Request in
    with_trans (List.fold_left (fun acc (table,keys,_) ->
                    match keys.time_key with
                    | None -> acc
                    | Some time_key ->
                       acc >>= fun () -> exec (Caqti_request.exec Caqti_type.ptime_span
                                                 (Printf.sprintf "DELETE FROM %s WHERE %s <= (now()::TIMESTAMP - ?::INTERVAL)"
                                                    table time_key)) cleanup_dur)
                  (return ()) M.tables)

  let delete_trans =
    let open Request in
    with_trans (List.fold_left (fun acc (table,_,_) ->
                    acc >>= fun () -> exec (Caqti_request.exec Caqti_type.unit
                                              (Printf.sprintf "DELETE FROM %s" table)) ())
                  (return ()) M.tables) 

  let request (state : t) req = pool_use state.db (fun db -> Request.run db req)
               
  let create (state : state) =
    let rec loop () =
      Lwt_unix.sleep state.period >>= (fun () ->
        match workers_trans with
        | None   -> Lwt.return_unit
        | Some w -> request state w)
      >>= fun () ->
      request state (cleanup_trans state.cleanup) >>= loop
    in
    Lwt_main.run (request state init_trans );
    Lwt.async loop;
    Ok state

  let delete state = request state delete_trans

end

module Types = struct
  include Caqti_type

  module List = struct
    type _ t = [] : unit t | (::) : 'a * 'b t -> ('a * 'b) t

    let (&) = Caqti_type.tup2
  end
              
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

                    
