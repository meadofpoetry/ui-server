module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Lwt)
open Config

type settings = { db_path : string } [@@deriving yojson]

let settings_default   = { db_path = "./db" }

let domain = "db"
           
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
    
module Make (C : CONFIG) : (DATABASE with type config = C.t ) = struct

  type config   = C.t
  type t        = Sqlexpr.db

  let get_settings = C.get settings_of_yojson domain settings_default 
              
  let create config =
    let cfg = get_settings config in 
    let dbs = Sqlexpr.open_db cfg.db_path (* ~mode:`NO_CREATE *)
    in dbs (* add maintenance later *)

  let insert db = Sqlexpr.insert db

  let select db = Sqlexpr.select db

  let finalize = Sqlexpr.close_db

end

