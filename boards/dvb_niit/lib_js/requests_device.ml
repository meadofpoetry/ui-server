open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let make_path = Boards_js.Requests.Device.make_path

module WS = struct

  include Boards_js.Requests.Device.WS

  let get_config control =
    let path = make_path control ["config"] in
    WS.get ~path config_of_yojson ()

end

module HTTP = struct

  include (Boards_js.Requests.Device.HTTP:
           module type of Boards_js.Requests.Device.HTTP
                          with module Archive := Boards_js.Requests.Device.HTTP.Archive)

  let post_reset control =
    let path = make_path control ["reset"] in
    post_result_unit ~path ()

  let get_config control =
    let path = make_path control ["config"] in
    get_result ~path config_of_yojson ()

  let get_devinfo control =
    let path = make_path control ["info"] in
    get_result ~path devinfo_opt_of_yojson ()

  module Archive = struct

    include Boards_js.Requests.Device.HTTP.Archive

  end

end
