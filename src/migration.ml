module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Lwt)

open Lwt.Infix
               
let create_users db =
  Sqlexpr.execute db [%sqlinit "CREATE TABLE IF NOT EXISTS users( \
                                type  INT2, \
                                login TEXT UNIQUE, \
                                password TEXT NON NULL, \
                                email TEXT \
                                );" ]
  >>= fun _ ->
  Sqlexpr.insert db [%sqlc "INSERT OR IGNORE INTO users(type,login,password) VALUES(0,'root','pswd')"]

let create_streams db =
  Sqlexpr.execute db [%sqlinit "CREATE TABLE IF NOT EXISTS streams( \
                                input  TEXT NON NULL, \
                                value  TEXT, \
                                date   TIMESTAMP DEFAULT CURRENT_TIMESTAMP \
                                );" ]
  
let create_errors db =
  Sqlexpr.execute db [%sqlinit "CREATE TABLE IF NOT EXISTS errors ( \
                                type  INTEGER, \
                                time  TIMESTAMP \
                                );" ]
 
let migrate db =
  create_users db >>= fun _ ->
  create_streams db >>= fun _ ->
  create_errors db
