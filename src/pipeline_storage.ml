open Common

open Database
open Lwt.Infix
   
let ( % ) = CCFun.(%)
   
type _ req =
  | Store_streams : Streams.t -> unit Lwt.t req
  
let store_streams dbs streams =
  let s = Streams_conv.dump_streams streams in
  List.fold_left
    (fun thread (i,v) ->
       thread >>= fun () ->
       Database.execute dbs
         [%sqlc "INSERT INTO streams(input, value) VALUES (%s, %s)"]
         i v )
    Lwt.return_unit s

let request (type a) dbs (r : a req) : a =
  match r with
  | Store_streams s -> store_streams dbs s
