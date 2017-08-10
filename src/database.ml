module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Lwt)
           
module type STORAGE = sig
  type t
  type _ req
  val  request : t -> 'a req -> 'a
end

module type DATABASE = sig
  type config
  type t
  val  create   : config -> t
  val  insert   : t -> ('a, int64 Sqlexpr.result) Sqlexpr.statement -> 'a
  val  select   : t -> ?batch:int -> ('c, 'a, 'a list Sqlexpr.result) Sqlexpr.expression -> 'c
  val  finalize : t -> unit
end

module Settings = struct
  type t = { db_path : string } [@@deriving yojson]
  let default   = { db_path = "./db" }
  let domain = "db"
end

module Conf = Config.Make(Settings)
 
type t        = Sqlexpr.db
                 
let create config =
  let cfg = Conf.get config in 
  let dbs = Sqlexpr.open_db cfg.db_path (* ~mode:`NO_CREATE *)
  in dbs (* add maintenance later *)

let insert db = Sqlexpr.insert db

let select db = Sqlexpr.select db

let finalize = Sqlexpr.close_db

