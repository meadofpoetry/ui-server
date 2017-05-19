open Lwt
open Containers
open User

module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Lwt)
open Sqlexpr
               
type database_settings = { path : string }

type database = { db_desc     : db
                ; tokens      : token_table
                }
              
let create settings =
  let dbs = { db_desc = Sqlexpr.open_db settings.path (* ~mode:`NO_CREATE *)
            ; tokens  = Tokentbl.create 200
            }
  in dbs (* add maintain later *)

let push_token dat tok =
  let hsh = hash_token tok in
  Tokentbl.add dat.tokens hsh tok;
  hsh

let lookup_token dat key =
  try Some (Tokentbl.find dat.tokens key)
  with Not_found -> None

let filter_expired_tokens dat =
  Tokentbl.filter_map_inplace (fun k tok -> if is_expired tok then None else Some tok) dat.tokens
