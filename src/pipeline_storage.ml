open Common

open Database
open Lwt.Infix
   
let ( % ) = CCFun.(%)
   
type _ req =
  | Store_streams : Streams.t -> unit Lwt.t req
  
let store_streams dbs streams =
  match (Streams_conv.dump_streams streams) with
  | None   -> Lwt.return_unit
  | Some s -> List.fold_left 
                (fun thread (i,v) ->
                  thread >>= fun () ->
                  Database.execute dbs
                                   [%sqlc "INSERT INTO streams(input, value) VALUES (%s, %s)"]
                                   i v )
                Lwt.return_unit s 
            
let request (type a) dbs (r : a req) : a =
  match r with
  | Store_streams s -> store_streams dbs s
