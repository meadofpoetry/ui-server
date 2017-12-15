open Containers
open Lwt.Infix
   
let (%) = Fun.(%)
        
include Common.User

module Storage : sig 
  type _ req =
    | Get_passwd : Common.User.t -> Common.User.pass Lwt.t req
    | Set_passwd : Common.User.pass -> unit Lwt.t req
    
  include (Storage.Database.STORAGE with type 'a req := 'a req)
end = User_storage

let init db =
  Lwt_main.run @@ Storage.init db

let validate dbs id pass =
  Common.User.of_string id
  |> function
    | Error e -> Lwt.return_error e
    | Ok user ->
       Lwt.catch
         (fun () -> Storage.request dbs (Storage.Get_passwd user))
         (fun e  -> Lwt_io.printf "DB user error: %s\n" (Printexc.to_string e) >>= fun () -> Lwt.fail e)
       >>= fun u ->
       if pass = u.password
       then Lwt.return_ok u.user
       else Lwt.return_error "ups"
