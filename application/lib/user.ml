open Containers
   
let (%) = Fun.(%)
        
include Common.User

type userdata = { pass : string
                } [@@deriving yojson]
    
type users = { root     : userdata
             ; operator : userdata
             ; guest    : userdata
             } [@@deriving yojson]

module Data = struct
  type t = users
  let default = { root     = { pass = "pswd" }
                ; operator = { pass = "default" }
                ; guest    = { pass = "default" }
                }
  (* TODO add encryption *)
  let dump = Yojson.Safe.to_string % users_to_yojson
  let restore = users_of_yojson % Yojson.Safe.from_string
end

module Config_storage = Storage.Options.Make (Data)

type entries = users Storage.Options.storage 

let get_pass storage = function
  | `Root     -> (storage#get).root
  | `Operator -> (storage#get).operator
  | `Guest    -> (storage#get).guest

let set_pass storage (pass_entry : pass) =
  let table = storage#get in
  match pass_entry.user with
  | `Root     -> storage#store { table with root     = { pass = pass_entry.password } }
  | `Operator -> storage#store { table with operator = { pass = pass_entry.password } }
  | `Guest    -> storage#store { table with guest    = { pass = pass_entry.password } }
             
let create config : entries =
  let stor = Storage.Options.Conf.get config in
  Config_storage.create stor.config_dir ["users"]
  
let validate storage id pass =
  Common.User.of_string id
  |> function
    | Error e -> Error (`Unknown e)
    | Ok user -> let actual_pass = get_pass storage user in
                 if String.equal pass actual_pass.pass
                 then Ok user
                 else Error `Wrong_password
