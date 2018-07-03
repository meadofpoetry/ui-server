open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path () = Boards_js.Requests.Device.get_device_path ()

module WS = struct

  open Common.Uri

  include Boards_js.Requests.Device.WS

  let get_ip control =
    WS.get ~from:Ipaddr.V4.of_yojson
      ~path:Path.Format.(get_base_path () / ("ip" @/ empty))
      ~query:Query.empty
      control

  let get_mask control =
    WS.get ~from:Ipaddr.V4.of_yojson
      ~path:Path.Format.(get_base_path () / ("mask" @/ empty))
      ~query:Query.empty
      control

  let get_gateway control =
    WS.get ~from:Ipaddr.V4.of_yojson
      ~path:Path.Format.(get_base_path () / ("gateway" @/ empty))
      ~query:Query.empty
      control

  let get_dhcp control =
    WS.get ~from:Json.Bool.of_yojson
      ~path:Path.Format.(get_base_path () / ("dhcp" @/ empty))
      ~query:Query.empty
      control

  let get_mode control =
    WS.get ~from:nw_of_yojson
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.empty
      control

end

module HTTP = struct

  open Common.Uri

  include (Boards_js.Requests.Device.HTTP:
           module type of Boards_js.Requests.Device.HTTP
                          with module Archive := Boards_js.Requests.Device.HTTP.Archive)

  let reset control =
    post_result_unit
      ~path:Path.Format.(get_base_path () / ("reset" @/ empty))
      ~query:Query.empty
      control

  let set_ip ip control =
    post_result ~from:Ipaddr.V4.of_yojson
      ~contents:(Ipaddr.V4.to_yojson ip)
      ~path:Path.Format.(get_base_path () / ("ip" @/ empty))
      ~query:Query.empty
      control

  let set_mask mask control =
    post_result ~from:Ipaddr.V4.of_yojson
      ~contents:(Ipaddr.V4.to_yojson mask)
      ~path:Path.Format.(get_base_path () / ("mask" @/ empty))
      ~query:Query.empty
      control

  let set_gateway gateway control =
    post_result ~from:Ipaddr.V4.of_yojson
      ~contents:(Ipaddr.V4.to_yojson gateway)
      ~path:Path.Format.(get_base_path () / ("gateway" @/ empty))
      ~query:Query.empty
      control

  let set_dhcp dhcp control =
    post_result ~from:Json.Bool.of_yojson
      ~contents:(Json.Bool.to_yojson dhcp)
      ~path:Path.Format.(get_base_path () / ("dhcp" @/ empty))
      ~query:Query.empty
      control

  let set_mode mode control =
    post_result ~from:nw_of_yojson
      ~contents:(nw_to_yojson mode)
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.empty
      control

  let get_devinfo control =
    get_result ~from:(Json.Option.of_yojson devinfo_of_yojson)
      ~path:Path.Format.(get_base_path () / ("info" @/ empty))
      ~query:Query.empty
      control

  let get_ip control =
    get_result ~from:Ipaddr.V4.of_yojson
      ~path:Path.Format.(get_base_path () / ("ip" @/ empty))
      ~query:Query.empty
      control

  let get_mask control =
    get_result ~from:Ipaddr.V4.of_yojson
      ~path:Path.Format.(get_base_path () / ("mask" @/ empty))
      ~query:Query.empty
      control

  let get_gateway control =
    get_result ~from:Ipaddr.V4.of_yojson
      ~path:Path.Format.(get_base_path () / ("gateway" @/ empty))
      ~query:Query.empty
      control

  let get_dhcp control =
    get_result ~from:Json.Bool.of_yojson
      ~path:Path.Format.(get_base_path () / ("dhcp" @/ empty))
      ~query:Query.empty
      control

  let get_mode control =
    get_result ~from:nw_of_yojson
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.empty
      control

  module Archive = struct

    include Boards_js.Requests.Device.HTTP.Archive

    let get_mode ?limit ?compress ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / ("mode/archive" @/ empty))
        ~query:Query.[ "limit",    (module Option(Int))
                     ; "compress", (module Option(Bool))
                     ; "from",     (module Option(Time.Show))
                     ; "to",       (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control limit compress from till duration

  end

end
