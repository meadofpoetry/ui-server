open Board_types.Device
open Api_js.Requests.Json_request
open Common

let get_base_path = Boards_js.Requests.Device.get_device_path

module WS = struct

  open Common.Uri

  include Boards_js.Requests.Device.WS

  let get_receivers control =
    WS.get ~from:Json.(Option.of_yojson @@ List.of_yojson Int.of_yojson)
      ~path:Path.Format.(get_base_path () / ("receivers" @/ empty))
      ~query:Query.empty
      control

  let get_mode ?(ids = []) control =
    WS.get ~from:config_of_yojson
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.["id", (module List(Int))]
      control ids

end

module HTTP = struct

  include (Boards_js.Requests.Device.HTTP:
           module type of Boards_js.Requests.Device.HTTP)

  open Common.Uri

  let reset control =
    post_result_unit
      ~path:Path.Format.(get_base_path () / ("reset" @/ empty))
      ~query:Query.empty control

  let set_mode ~id mode control =
    let contents = mode_to_yojson mode in
    post_result ~contents
      ~from:Json.(Pair.of_yojson Int.of_yojson mode_rsp_of_yojson)
      ~path:Path.Format.(get_base_path () / ("mode" @/ Int ^/ empty))
      ~query:Query.empty
      control id

  let get_devinfo control =
    get_result ~from:(Json.Option.of_yojson devinfo_of_yojson)
      ~path:Path.Format.(get_base_path () / ("info" @/ empty))
      ~query:Query.empty
      control

  let get_receivers control =
    get_result ~from:Json.(Option.of_yojson @@ List.of_yojson Int.of_yojson)
      ~path:Path.Format.(get_base_path () / ("receivers" @/ empty))
      ~query:Query.empty
      control

  let get_mode ?(ids = []) control =
    get_result ~from:config_of_yojson
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.["id", (module List(Int))]
      control ids

end
