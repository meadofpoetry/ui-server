open Common.User
open Database
   
open Lwt.Infix

type _ req =
  | Get_passwd : Common.User.t -> Common.User.pass Lwt.t req
  | Set_passwd : Common.User.pass -> unit Lwt.t req

let get_passwd dbs id =
  Database.select_one dbs [%sql "SELECT @s{password} FROM users WHERE type = %d"]
                      (to_int id)
  >>= fun p ->
  Lwt.return { user = id; password = p } 

let set_passwd dbs p =
  Database.execute dbs [%sql "UPDATE users SET password = %s WHERE type = %d"]
                   p.password (to_int p.user)
  
let request (type a) dbs (r : a req) : a =
  let open Database in
  match r with
  | Get_passwd id -> get_passwd dbs id
  | Set_passwd p  -> set_passwd dbs p
