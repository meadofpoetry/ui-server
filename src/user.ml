open Containers

let (%) = Fun.(%)

include Common.User

module Storage : sig 
  type _ req =
    | Get_passwd : Common.User.t -> Common.User.pass Lwt.t req
    | Set_passwd : Common.User.pass -> unit Lwt.t req
    
  include (Database.STORAGE with type 'a req := 'a req)
end = User_storage

let init db =
  Lwt_main.run @@ Storage.init db
