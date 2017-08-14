module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Lwt)
open Sqlexpr

module Settings = struct
  type t = { db_path : string } [@@deriving yojson]
  let default   = { db_path = "./db" }
  let domain = "db"
end

module Conf = Config.Make(Settings)
 
type t        = Sqlexpr.db

module type STORAGE = sig
  type _ req
  val  request : t -> 'a req -> 'a
end
                 
let create config =
  let cfg = Conf.get config in
  let dbs = Sqlexpr.open_db cfg.db_path (* ~mode:`NO_CREATE *) in
  Migration.migrate dbs |> ignore;
  dbs (* add maintenance later *)

let insert db = Sqlexpr.insert db

let execute db = Sqlexpr.execute db

let select db = Sqlexpr.select db

let select_one db = Sqlexpr.select_one db

let finalize = Sqlexpr.close_db

