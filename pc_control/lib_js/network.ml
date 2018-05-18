open Components
open Common.User
open Api_js.Requests.Json_request

module Requests = struct

  let get_config () =
    get_result Network_config.of_yojson "api/network/config"

  let post_config conf =
    post_result ~contents:(Network_config.to_yojson conf) (fun _ -> Ok ()) "api/network/config"
    
end

let make_card is_root config =
  
  let apply       = new Button.t ~label:"Применить" () in
  ()
