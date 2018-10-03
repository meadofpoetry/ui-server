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
  val key : ?default:string -> ?primary:bool -> string -> t
  val is_primary : t -> bool
  val typ : t -> string
  val to_string : t -> string
end = struct
  type t =
    { typ : string
    ; default : string option
    ; primary : bool
    }
  let key ?default ?(primary = false) (typ : string) : t =
    { typ; default; primary }
  let typ (t : t) = t.typ
  let is_primary (t : t) = t.primary
  let to_string (t : t) : string = match t.default with
    | Some def -> t.typ ^ " DEFAULT " ^ def (* TODO add check *)
    | None -> t.typ
end

type keys =
  { columns  : (string * Key_t.t) list
  ; time_key : string option
  }

let make_keys ?time_key columns =
  { columns; time_key }
          
module type MODEL = sig
  type init
  type names
  val name     : string
  val tables   : init -> names * ((string * keys * (unit, _) Request.t option) list)
end
           
module type CONN = sig
  type t
  type init
  type names
  val create   : state -> init -> (t, string) result
  val request  : t -> ('req,_) Request.t -> 'req Lwt.t
  val delete   : t -> unit Lwt.t
  val names    : t -> names
end

module Make (M : MODEL) : (CONN with type init := M.init and type names := M.names) = struct

  type t = { state:  state
           ; tables: (string * keys * (unit,[`Simple]) Request.t option) list
           ; names:  M.names
           } 

  let make_init_query name keys =
    let primary =
      List.filter_map (fun (k, t) ->
          if Key_t.is_primary t then Some k else None) keys
      |> function
        | [] -> None
        | s -> Some (Printf.sprintf "PRIMARY KEY (%s)" @@ String.concat "," s) in
    let cols = String.concat ", " (List.map (fun (k,t) -> k ^ " " ^ (Key_t.to_string t)) keys) in
    let cols = match primary with
      | None -> cols
      | Some s -> cols ^ ", " ^ s in
    let exp = Printf.sprintf "CREATE TABLE IF NOT EXISTS %s (%s)" name cols in
    Request.exec (Caqti_request.exec Caqti_type.unit exp)

  let init_trans tables =
    let open Request in
    with_trans (List.fold_left (fun acc (name,keys,_) -> acc >>= make_init_query name keys.columns)
                  (return ()) tables)

  let workers_trans tables =
    let open Request in
    List.filter_map (fun (_,_,w) -> w) tables
    |> function [] -> None
              | lst -> Some (with_trans (List.fold_left (fun acc m -> acc >>= fun () -> m) (return ()) lst))

  (* TODO sprintf *)
  let cleanup_trans tables cleanup_dur =
    let open Request in
    with_trans (
        List.fold_left (fun acc (table,keys,_) ->
            match keys.time_key with
            | None -> acc
            | Some time_key ->
               acc >>= fun () ->
               exec (Caqti_request.exec Caqti_type.ptime_span
                       (Printf.sprintf "DELETE FROM %s \
                                        WHERE %s <= (now()::TIMESTAMP - ?::INTERVAL)"
                          table time_key)) cleanup_dur)
          (return ()) tables)

  let delete_trans tables =
    let open Request in
    with_trans (List.fold_left (fun acc (table,_,_) ->
                    acc >>= fun () -> exec (Caqti_request.exec Caqti_type.unit
                                              (Printf.sprintf "DELETE FROM %s" table)) ())
                  (return ()) tables) 

  (* FIXME remove log *)
  let request (state : t) req =
    pool_use state.state.db (fun db -> Request.run db req)
    |> fun x -> Lwt.catch (fun () -> x)
                  (fun e -> Logs.err (fun m -> m "DB ERR: %s" @@ Printexc.to_string e);
                            raise e)
               
  let create (state : state) sign =
    let names, tables = M.tables sign in
    let obj = { state; tables; names } in
    (* TODO cleanup at startup *)
    let rec loop () =
      begin match workers_trans tables with
      | None   -> Lwt.return_unit
      | Some w -> request obj w
      end
      >>= fun () -> request obj (cleanup_trans tables obj.state.cleanup)
      >>= fun () -> Lwt_unix.sleep obj.state.period
      >>= loop
    in
    Lwt_main.run (request obj @@ init_trans tables);
    Lwt.async loop;
    Ok obj

  let delete obj = request obj (delete_trans obj.tables)

  let names obj = obj.names

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

                    
