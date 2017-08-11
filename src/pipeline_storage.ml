open Common

open Database
open Lwt.Infix
   
let ( % ) = CCFun.(%)
   
type _ req =
  | Store_opts : Options.t -> int64 Lwt.t req

let store_opts dbs opts =
  let dump = Msg_conv.to_string % Options.to_yojson in
  Database.insert dbs [%sql "INSERT INTO streams(stream) VALUES (%s)"]
                      (dump opts)
  
let request (type a) dbs (r : a req) : a =
  match r with
  | Store_opts o -> store_opts dbs o
