open Lwt
open Sqlite3
open Cryptokit
open Containers
open User

let magic_key = "42_magic_42"
   
type database_settings = { path : string }

type database = { db_desc     : db
                ; hash        : hash
                ; tokens      : token_table
                }
              
let create settings =
  let dbs = { db_desc = Sqlite3.db_open settings.path (* ~mode:`NO_CREATE *)
            ; tokens  = Tokentbl.create 200
            ; hash    = MAC.hmac_sha256 magic_key
            }
  in dbs (* add maintain later *)

let push_token dat tok =
  let hsh = hash_token dat.hash tok in
  Tokentbl.add dat.tokens hsh tok;
  hsh

let lookup_token dat key =
  try Some (Tokentbl.find dat.tokens key)
  with Not_found -> None

let filter_expired_tokens dat =
  Tokentbl.filter_map_inplace (fun k tok -> if is_expired tok then None else Some tok) dat.tokens
