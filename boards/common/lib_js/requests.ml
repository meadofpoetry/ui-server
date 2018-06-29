open Containers
open Api_js.Requests.Json_request
open Lwt.Infix
open Common

let get_board_path () = Uri.Path.Format.("/api/board" @/ Int ^/ empty)

module Device = struct

  let get_device_path () = Uri.Path.Format.(get_board_path () / ("device" @/ empty))

  module WS = struct

    let get_state control =
      WS.get ~from:Topology.state_of_yojson
        ~path:Uri.Path.Format.(get_device_path () / ("state" @/ empty))
        ~query:Uri.Query.empty control

  end

  module HTTP = struct

    (** Sets board port to listen **)
    let post_port ~port ~state control =
      post_result_unit
        ~path:Uri.Path.Format.(get_device_path () / ("port" @/ Int ^/ Bool ^/ empty))
        ~query:Uri.Query.empty
        control port state

    let get_state control =
      get_result ~from:Topology.state_of_yojson
        ~path:Uri.Path.Format.(get_device_path () / ("state" @/ empty))
        ~query:Uri.Query.empty
        control

    module Archive = struct

      let get_state ?limit ?compress ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "not implemented")
                   ~path:Uri.Path.Format.(get_device_path () / ("state/archive" @/ empty))
                   ~query:Uri.Query.[ "limit",    (module Option(Int))
                                    ; "compress", (module Option(Bool))
                                    ; "from",     (module Option(Time.Show))
                                    ; "to",       (module Option(Time.Show))
                                    ; "duration", (module Option(Time.Relative))]
                   control limit compress from till duration

    end

  end

end
