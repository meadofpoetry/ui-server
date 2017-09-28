open Lwt.Infix

module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Lwt)
open Sqlexpr
   
module Settings = struct
  type t = { db_path : string } [@@deriving yojson]
  let default   = { db_path = "./db" }
  let domain = "db"
end

module Conf = Config.Make(Settings)
 
type t        = { db      : Sqlexpr.db
                ; workers : (t -> unit Lwt.t) list ref
                }

module type STORAGE = sig
  type _ req
  val init     : t -> unit Lwt.t
  val request  : t -> 'a req -> 'a
end
                 
let create config period =
  let cfg = Conf.get config in
  let db  = Sqlexpr.open_db cfg.db_path (* ~mode:`NO_CREATE *) in
  let workers = ref [] in
  let obj = { db; workers } in
  let rec loop () =
    let rec traverse = function
      | [] -> Lwt.return_unit
      | x::tl -> x obj >>= fun () -> traverse tl
    in
    Lwt_unix.sleep period >>= fun () ->
    traverse !workers     >>= loop
  in
  obj, loop ()

let insert o = Sqlexpr.insert o.db

let execute o = Sqlexpr.execute o.db

let select o = Sqlexpr.select o.db

let select_one o = Sqlexpr.select_one o.db

let add_maintainer o f =
  o.workers := f :: !(o.workers)

let finalize o =
  print_endline "closing db";
  Sqlexpr.close_db o.db;
