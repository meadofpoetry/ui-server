open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path () = Uri.Path.Format.(Boards_js.Requests.get_board_path () / ("receiver" @/ empty))

module WS = struct

  open Common.Uri

  let get_enabled control =
    WS.get ~from:Json.Bool.of_yojson
      ~path:Path.Format.(get_base_path () / ("enabled" @/ empty))
      ~query:Query.empty
      control

  let get_fec control =
    WS.get ~from:Json.Bool.of_yojson
      ~path:Path.Format.(get_base_path () / ("fec" @/ empty))
      ~query:Query.empty
      control

  let get_port control =
    WS.get ~from:Json.Int.of_yojson
      ~path:Path.Format.(get_base_path () / ("port" @/ empty))
      ~query:Query.empty
      control

  let get_meth control =
    WS.get ~from:meth_of_yojson
      ~path:Path.Format.(get_base_path () / ("method" @/ empty))
      ~query:Query.empty
      control

  let get_multicast control =
    WS.get ~from:Ipaddr.V4.of_yojson
      ~path:Path.Format.(get_base_path () / ("multicast" @/ empty))
      ~query:Query.empty
      control

  let get_delay control =
    WS.get ~from:Json.Int.of_yojson
      ~path:Path.Format.(get_base_path () / ("delay" @/ empty))
      ~query:Query.empty
      control

  let get_rate_mode control =
    WS.get ~from:rate_mode_of_yojson
      ~path:Path.Format.(get_base_path () / ("rate-mode" @/ empty))
      ~query:Query.empty
      control

  let get_mode control =
    WS.get ~from:ip_of_yojson
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.empty
      control

  let get_status control =
    WS.get ~from:status_of_yojson
      ~path:Path.Format.(get_base_path () / ("status" @/ empty))
      ~query:Query.empty
      control

end

module HTTP = struct

  open Common.Uri

  let set_enabled enable control =
    post_result ~from:Json.Bool.of_yojson
      ~contents:(Json.Bool.to_yojson enable)
      ~path:Path.Format.(get_base_path () / ("enabled" @/ empty))
      ~query:Query.empty
      control

  let set_fec fec control =
    post_result ~from:Json.Bool.of_yojson
      ~contents:(Json.Bool.to_yojson fec)
      ~path:Path.Format.(get_base_path () / ("fec" @/ empty))
      ~query:Query.empty
      control

  let set_port port control =
    post_result ~from:Json.Int.of_yojson
      ~contents:(Json.Int.to_yojson port)
      ~path:Path.Format.(get_base_path () / ("port" @/ empty))
      ~query:Query.empty
      control

  let set_meth meth control =
    post_result ~from:meth_of_yojson
      ~contents:(meth_to_yojson meth)
      ~path:Path.Format.(get_base_path () / ("method" @/ empty))
      ~query:Query.empty
      control

  let set_multicast multicast control =
    post_result ~from:Ipaddr.V4.of_yojson
      ~contents:(Ipaddr.V4.to_yojson multicast)
      ~path:Path.Format.(get_base_path () / ("multicast" @/ empty))
      ~query:Query.empty
      control

  (* let set_delay delay control =
   *   post_result ~from:Json.Int.of_yojson
   *     ~contents:(Json.Int.to_yojson delay)
   *     ~path:Path.Format.(get_base_path () / ("delay" @/ empty))
   *     ~query:Query.empty
   *     control *)

  let set_rate_mode rate_mode control =
    post_result ~from:rate_mode_of_yojson
      ~contents:(rate_mode_to_yojson rate_mode)
      ~path:Path.Format.(get_base_path () / ("rate-mode" @/ empty))
      ~query:Query.empty
      control

  let set_mode mode control =
    post_result ~from:ip_of_yojson
      ~contents:(ip_to_yojson mode)
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.empty
      control

  let get_enabled control =
    get_result ~from:Json.Bool.of_yojson
      ~path:Path.Format.(get_base_path () / ("enabled" @/ empty))
      ~query:Query.empty
      control

  let get_fec control =
    get_result ~from:Json.Bool.of_yojson
      ~path:Path.Format.(get_base_path () / ("fec" @/ empty))
      ~query:Query.empty
      control

  let get_port control =
    get_result ~from:Json.Int.of_yojson
      ~path:Path.Format.(get_base_path () / ("port" @/ empty))
      ~query:Query.empty
      control

  let get_meth control =
    get_result ~from:meth_of_yojson
      ~path:Path.Format.(get_base_path () / ("method" @/ empty))
      ~query:Query.empty
      control

  let get_multicast control =
    get_result ~from:Ipaddr.V4.of_yojson
      ~path:Path.Format.(get_base_path () / ("multicast" @/ empty))
      ~query:Query.empty
      control

  let set_delay control =
    get_result ~from:Json.Int.of_yojson
      ~path:Path.Format.(get_base_path () / ("delay" @/ empty))
      ~query:Query.empty
      control

  let get_rate_mode control =
    get_result ~from:rate_mode_of_yojson
      ~path:Path.Format.(get_base_path () / ("rate-mode" @/ empty))
      ~query:Query.empty
      control

  let get_mode control =
    get_result ~from:ip_of_yojson
      ~path:Path.Format.(get_base_path () / ("mode" @/ empty))
      ~query:Query.empty
      control

  let get_status control =
    get_result ~from:(Json.Option.of_yojson status_of_yojson)
      ~path:Path.Format.(get_base_path () / ("status" @/ empty))
      ~query:Query.empty
      control

  module Archive = struct
    let get_status ?limit ?compress ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / ("status/archive" @/ empty))
        ~query:Query.[ "limit", (module Option(Int))
                     ; "compress", (module Option(Bool))
                     ; "from", (module Option(Time.Show))
                     ; "to", (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control limit compress from till duration

    let get_mode ?limit ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / ("mode/archive" @/ empty))
        ~query:Query.[ "limit", (module Option(Int))
                     ; "from", (module Option(Time.Show))
                     ; "to", (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control limit from till duration
  end

end
