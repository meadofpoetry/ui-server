module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Lwt)
open Sqlexpr
               
type settings = { path : string }

type t = { db_desc     : db
         }
              
let create settings =
  let dbs = { db_desc = Sqlexpr.open_db settings.path (* ~mode:`NO_CREATE *)
            }
  in dbs (* add maintain later *)
