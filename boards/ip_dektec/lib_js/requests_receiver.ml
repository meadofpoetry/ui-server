open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let make_path control path = Boards_js.Requests.make_path control ("receiver"::path)

module WS = struct

  let get_enable control =
    let path = make_path control ["enable"] in
    WS.get ~path flag_of_yojson ()

  let get_fec control =
    let path = make_path control ["fec"] in
    WS.get ~path flag_of_yojson ()

  let get_port control =
    let path = make_path control ["port"] in
    WS.get ~path port_of_yojson ()

  let get_meth control =
    let path = make_path control ["method"] in
    WS.get ~path meth_of_yojson ()

  let get_multicast control =
    let path = make_path control ["multicast"] in
    WS.get ~path ipv4_opt_of_yojson ()

  let get_delay control =
    let path = make_path control ["delay"] in
    WS.get ~path delay_of_yojson ()

  let get_rate_mode control =
    let path = make_path control ["rate-mode"] in
    WS.get ~path rate_mode_of_yojson ()

  let get_mode control =
    let path = make_path control ["mode"] in
    WS.get ~path ip_of_yojson ()

  let get_status control =
    let path = make_path control ["status"] in
    WS.get ~path status_of_yojson ()

end

module HTTP = struct

  let post_enable control enable =
    let path     = make_path control ["enable"] in
    let contents = flag_to_yojson enable in
    post_result ~path ~contents flag_of_yojson ()

  let post_fec control fec =
    let path     = make_path control ["fec"] in
    let contents = flag_to_yojson fec in
    post_result ~path ~contents flag_of_yojson ()

  let post_port control port =
    let path     = make_path control ["port"] in
    let contents = port_to_yojson port in
    post_result ~path ~contents port_of_yojson ()

  let post_meth control meth =
    let path     = make_path control ["method"] in
    let contents = meth_to_yojson meth in
    post_result ~path ~contents meth_of_yojson ()

  let post_multicast control multicast =
    let path     = make_path control ["multicast"] in
    let contents = Ipaddr.V4.to_yojson multicast in
    post_result ~path ~contents Ipaddr.V4.of_yojson ()

  let post_delay control delay =
    let path     = make_path control ["delay"] in
    let contents = delay_to_yojson delay in
    post_result ~path ~contents delay_of_yojson ()

  let post_rate_mode control rate_mode =
    let path     = make_path control ["rate-mode"] in
    let contents = rate_mode_to_yojson rate_mode in
    post_result ~path ~contents rate_mode_of_yojson ()

  let post_mode control mode =
    let path     = make_path control ["mode"] in
    let contents = ip_to_yojson mode in
    post_result ~path ~contents ip_of_yojson ()

  let get_enable control =
    let path = make_path control ["enable"] in
    get_result ~path flag_of_yojson ()

  let get_fec control =
    let path = make_path control ["fec"] in
    get_result ~path flag_of_yojson ()

  let get_port control =
    let path = make_path control ["port"] in
    get_result ~path port_of_yojson ()

  let get_meth control =
    let path = make_path control ["method"] in
    get_result ~path meth_of_yojson ()

  let get_multicast control =
    let path = make_path control ["multicast"] in
    get_result ~path flag_of_yojson ()

  let get_delay control =
    let path = make_path control ["delay"] in
    get_result ~path delay_of_yojson ()

  let get_rate_mode control =
    let path = make_path control ["rate-mode"] in
    get_result ~path rate_mode_of_yojson ()

  let get_mode control =
    let path = make_path control ["mode"] in
    get_result ~path ip_of_yojson ()

  module Archive = struct

  end

end
