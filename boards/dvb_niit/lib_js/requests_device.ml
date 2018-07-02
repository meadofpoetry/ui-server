open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path = Boards_js.Requests.Device.get_device_path

module WS = struct

  open Common.Uri

  include Boards_js.Requests.Device.WS

  let get_config control =
    let path = Path.Format.(get_base_path () / ("config" @/ empty)) in
    WS.get ~from:config_of_yojson ~path ~query:Query.empty control

end

module HTTP = struct

  include (Boards_js.Requests.Device.HTTP:
           module type of Boards_js.Requests.Device.HTTP
                          with module Archive := Boards_js.Requests.Device.HTTP.Archive)

  open Common.Uri

  let post_reset control =
    post_result_unit
      ~path:Path.Format.(get_base_path () / ("reset" @/ empty))
      ~query:Query.empty control

  let get_devinfo control =
    get_result ~from:(Json.Option.of_yojson devinfo_of_yojson)
      ~path:Path.Format.(get_base_path () / ("info" @/ empty))
      ~query:Query.empty
      control

  let get_config control =
    get_result ~from:config_of_yojson
      ~path:Path.Format.(get_base_path () / ("config" @/ empty))
      ~query:Query.empty control

  module Archive = struct

    include Boards_js.Requests.Device.HTTP.Archive

  end

end
