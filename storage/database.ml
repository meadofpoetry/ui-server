open Containers
open Lwt.Infix
open Common
   
module Settings = struct
  type t = { socket_path : string; cleanup : Time.Period.Hours.t; password : string } [@@deriving yojson]
  let default   = { socket_path = "/tmp"; cleanup = Time.Period.Hours.of_hours 1; password = "ats3" }
  let domain = "db"
end
                
module Conf = Config.Make(Settings)

let pool_use p f =
  Caqti_lwt.Pool.use (fun c -> f c >>= Lwt.return_ok) p >>= function
  | Error e -> Lwt.fail_with (Caqti_error.show e)
  | Ok    v -> Lwt.return v

type state = { period  : float
             ; cleanup : Time.Period.Hours.t
             ; db      : ((module Caqti_lwt.CONNECTION), Caqti_error.connect) Caqti_lwt.Pool.t
             }

           (*
module type MODEL = sig
  type _ req
  val name     : string
  val table    : string
  val request  : (module Caqti_lwt.CONNECTION) -> 'a req -> 'a Lwt.t
                                                            *)
           
module type MODEL = sig
  type _ req
  val name     : string
  val init     : (module Caqti_lwt.CONNECTION) -> unit Lwt.t
  val request  : (module Caqti_lwt.CONNECTION) -> 'a req -> 'a Lwt.t
  val cleanup  : Time.Period.Hours.t -> (module Caqti_lwt.CONNECTION) -> unit Lwt.t
  val delete   : (module Caqti_lwt.CONNECTION) -> unit Lwt.t
  val worker   : ((module Caqti_lwt.CONNECTION) -> unit Lwt.t) option
end
           
module type CONN = sig
  type t
  type _ req
  val create   : state -> (t, string) result
  val request  : t -> 'a req -> 'a Lwt.t
end

module Make (M : MODEL) : (CONN with type 'a req := 'a M.req) = struct
  type t = state

  let create (state : state) =
    let rec loop () =
      Lwt_unix.sleep state.period >>= (fun () ->
        match M.worker with
        | None   -> Lwt.return_unit
        | Some w -> pool_use state.db w) >>= fun () ->
      pool_use state.db (M.cleanup state.cleanup) >>= loop
    in
    Lwt_main.run (pool_use state.db M.init);
    Lwt.async loop;
    Ok state

  let request (type a) state (req : a M.req) : a Lwt.t =
    pool_use state.db (fun c -> M.request c req)
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

let error s e =
  Printf.sprintf s (Caqti_error.show e)
