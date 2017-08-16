open Containers

let (%) = Fun.(%)

include Common.User

module Storage : sig 
  type _ req =
    | Get_pass   : string -> pass Lwt.t req
    | Get_user   : string -> info Lwt.t req 
    | Get_users  : info list Lwt.t req
    | Set_passwd : pass -> unit Lwt.t req
    | Set_info   : info -> unit Lwt.t req
    | Set_typ    : user -> unit Lwt.t req
    | Add_user   : full -> int64 Lwt.t req
    | Rm_user    : string -> unit Lwt.t req
    
  include (Database.STORAGE with type 'a req := 'a req)
end = User_storage
