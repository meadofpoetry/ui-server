open Containers
open Storage.Database
open Lwt.Infix
      
type _ req =
  | Store_structures : Structure.t list -> unit Lwt.t req

let init o =
  Storage.Database.execute o [%sqlinit "CREATE TABLE IF NOT EXISTS streams( \
                                        input  TEXT NON NULL, \
                                        value  TEXT, \
                                        date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP \
                                        );" ]
  >>= fun _ -> Lwt.return_unit
             
let store_structures dbs streams =
  let s = Structure_conv.dump_streams streams in
  List.fold_left
    (fun thread (i,v) ->
       thread >>= fun () ->
       Storage.Database.execute dbs
         [%sqlc "INSERT INTO streams(input, value) VALUES (%s, %s)"]
         i v )
    Lwt.return_unit s

let request (type a) dbs (r : a req) : a =
  match r with
  | Store_structures s -> store_structures dbs s
