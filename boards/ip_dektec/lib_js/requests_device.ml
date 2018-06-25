open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let make_path = Boards_js.Requests.Device.make_path

module WS = struct

  include Boards_js.Requests.Device.WS

  let get_ip control =
    let path = make_path control ["ip"] in
    WS.get ~path Ipaddr.V4.of_yojson ()

  let get_mask control =
    let path = make_path control ["mask"] in
    WS.get ~path Ipaddr.V4.of_yojson ()

  let get_gateway control =
    let path = make_path control ["gateway"] in
    WS.get ~path Ipaddr.V4.of_yojson ()

  let get_dhcp control =
    let path = make_path control ["dhcp"] in
    WS.get ~path flag_of_yojson ()

  let get_mode control =
    let path = make_path control ["mode"] in
    WS.get ~path nw_of_yojson ()

end

module HTTP = struct

  include (Boards_js.Requests.Device.HTTP:
           module type of Boards_js.Requests.Device.HTTP
                          with module Archive := Boards_js.Requests.Device.HTTP.Archive)

  let post_reset control =
    let path = make_path control ["reset"] in
    post_result_unit ~path ()

  let post_ip control ip =
    let path     = make_path control ["ip"] in
    let contents = Ipaddr.V4.to_yojson ip in
    post_result ~path ~contents Ipaddr.V4.of_yojson ()

  let post_mask control mask =
    let path     = make_path control ["mask"] in
    let contents = Ipaddr.V4.to_yojson mask in
    post_result ~path ~contents Ipaddr.V4.of_yojson ()

  let post_gateway control gateway =
    let path     = make_path control ["gateway"] in
    let contents = Ipaddr.V4.to_yojson gateway in
    post_result ~path ~contents Ipaddr.V4.of_yojson ()

  let post_dhcp control dhcp =
    let path     = make_path control ["dhcp"] in
    let contents = flag_to_yojson dhcp in
    post_result ~path ~contents flag_of_yojson ()

  let post_mode control mode =
    let path     = make_path control ["mode"] in
    let contents = nw_to_yojson mode in
    post_result ~path ~contents nw_of_yojson ()

  let get_ip control =
    let path = make_path control ["ip"] in
    get_result ~path Ipaddr.V4.of_yojson ()

  let get_mask control =
    let path = make_path control ["mask"] in
    get_result ~path Ipaddr.V4.of_yojson ()

  let get_gateway control =
    let path = make_path control ["gateway"] in
    get_result ~path Ipaddr.V4.of_yojson ()

  let get_dhcp control =
    let path = make_path control ["dhcp"] in
    get_result ~path flag_of_yojson ()

  let get_mode control =
    let path = make_path control ["mode"] in
    get_result ~path nw_of_yojson ()

  module Archive = struct

    include Boards_js.Requests.Device.HTTP.Archive

  end

end
