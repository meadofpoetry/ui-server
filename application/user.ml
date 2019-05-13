include Application_types.User

type userdata = { pass : string
                } [@@deriving yojson, eq]
    
type users = { root     : userdata
             ; operator : userdata
             ; guest    : userdata
             } [@@deriving yojson, eq]

module Config = struct
  type t = users
  let default = { root     = { pass = "pswd" }
                ; operator = { pass = "default" }
                ; guest    = { pass = "default" }
                }
  let equal = equal_users
  (* TODO add encryption *)
  let to_string x =
    users_to_yojson x
    |> Yojson.Safe.to_string

  let of_string s =
    Yojson.Safe.from_string s
    |> users_of_yojson
    |> function Ok v -> v | Error e -> failwith e
end

module Passwd = Kv_v.RW (Config)
              
type passwd = Passwd.t

let get_pass (storage : passwd) user =
  let open Lwt.Infix in
  storage#get >>= fun passwd ->
  match user with
  | `Root     -> Lwt.return passwd.root
  | `Operator -> Lwt.return passwd.operator
  | `Guest    -> Lwt.return passwd.guest

let set_pass (storage : passwd) (pass_entry : pass) =
  let open Lwt.Infix in
  storage#get >>= fun table ->
  match pass_entry.user with
  | `Root     -> storage#set { table with root     = { pass = pass_entry.password } }
  | `Operator -> storage#set { table with operator = { pass = pass_entry.password } }
  | `Guest    -> storage#set { table with guest    = { pass = pass_entry.password } }
             
let create (config : Kv.RW.t) =
  Passwd.create ~default:Config.default config ["users"]
  
let validate (storage : passwd) ~name ~pass =
  let open Lwt.Infix in
  Application_types.User.of_string name
  |> function
    | Error e -> Lwt.return_error (`Unknown e)
    | Ok user ->
       get_pass storage user >>= fun actual_pass ->
       if String.equal pass actual_pass.pass
       then Lwt.return_ok user
       else Lwt.return_error `Wrong_password
