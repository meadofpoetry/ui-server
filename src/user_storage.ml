open Common.Userl
open Database
   
open Lwt.Infix

type _ req =
  | Get_pass   : string -> pass Lwt.t req
  | Get_user   : string -> info Lwt.t req 
  | Get_users  : info list Lwt.t req
  | Set_passwd : pass -> unit Lwt.t req
  | Set_info   : info -> unit Lwt.t req
  | Set_typ    : user -> unit Lwt.t req
  | Add_user   : full -> int64 Lwt.t req
  | Rm_user    : string -> unit Lwt.t req

let get_pass dbs name =
  Database.select_one dbs [%sql "SELECT @d{type}, @s{password} FROM users WHERE login = %s"]
                      name
  >>= fun (t,p) ->
  Lwt.return { user = { name; typ = (of_int t)}; password = p } 

let get_user dbs name =
  Database.select_one dbs [%sql "SELECT @d{type}, @s?{email} FROM users WHERE login = %s"]
                      name
  >>= fun (t,e) ->
  Lwt.return { user = { name; typ = (of_int t)}; email = e }

let get_users dbs =
  Database.select dbs [%sql "SELECT @d{type}, @s{login}, @s?{email} FROM users"]
  >|= List.map (fun (t,name,e) -> { user = { name; typ = (of_int t)}; email = e } )

let set_passwd dbs p =
  Database.execute dbs [%sql "UPDATE users SET password = %s WHERE login = %s"]
                   p.password p.user.name

let set_info dbs info =
  if info.user.name = "root" then Lwt.return_unit
  else Database.execute dbs [%sql "UPDATE users SET email = %s? WHERE login = %s"]
                        info.email info.user.name

let set_typ dbs typ =
  Database.execute dbs [%sql "UPDATE users SET type = %d,  WHERE login = %s"]
                   (to_int typ.typ) typ.name

let add_user dbs (data : full) =
  Database.insert dbs [%sql "INSERT OR IGNORE INTO users(type,login,password,email) VALUES(%d,%s,%s,%s?)"]
                  (to_int data.user.typ) data.user.name data.password data.email

let rm_user dbs name =
  if name = "root" then Lwt.return_unit
  else Database.execute dbs [%sql "DELETE FROM users WHERE login = %s"] name
  
let request (type a) dbs (r : a req) : a =
  let open Database in
  match r with
  | Get_pass name -> get_pass dbs name
  | Get_user name -> get_user dbs name
  | Get_users     -> get_users dbs
  | Set_passwd p  -> set_passwd dbs p
  | Set_info info -> set_info dbs info
  | Set_typ  typ  -> set_typ dbs typ
  | Add_user data -> add_user dbs data
  | Rm_user  name -> rm_user dbs name
