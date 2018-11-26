open Containers
open Board_types
open Api_js.Requests.Json_request
open Api_js.Api_types
open Common
open Lwt_result.Infix

let get_base_path () = Uri.Path.Format.(
    Boards_js.Requests.get_board_path () / ("history" @/ empty))

module HTTP = struct

  open Common.Uri
  open Stream

  module Streams = struct

    open Streams

    let get ?(ids = []) ?(inputs = []) ?limit ?incoming
          ?compress ?from ?till ?duration control =
      get_result ~from:(Api_js.Api_types.rows_of_yojson
                          streams_states_of_yojson
                          streams_unique_of_yojson)
        ~path:Path.Format.(get_base_path () / ("streams" @/ empty))
        ~query:Query.[ "id", (module List(Stream.ID))
                     ; "input", (module List(Topology.Show_topo_input))
                     ; "limit", (module Option(Int))
                     ; "incoming", (module Option(Bool))
                     ; "compress", (module Option(Bool))
                     ; "from", (module Option(Time.Show))
                     ; "to", (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control ids inputs limit incoming compress from till duration

  end

  module Errors = struct

    open Error

    (* let get ?(errors = []) ?(priority = []) ?(pids = []) ?(ids = [])
     *       ?limit ?order ?compress ?from ?till ?duration control =
     *   let desc = Option.map (function `Asc -> false | `Desc -> true) order in
     *   get_result ~from:(rows_of_yojson raw_of_yojson compressed_of_yojson)
     *     ~path:Path.Format.(get_base_path () / ("errors" @/ empty))
     *     ~query:Query.[ "id", (module List(Stream.ID))
     *                  ; "errors", (module List(Int))
     *                  ; "priority", (module List(Int))
     *                  ; "pid", (module List(Int))
     *                  ; "limit", (module Option(Int))
     *                  ; "desc", (module Option(Bool))
     *                  ; "compress", (module Option(Bool))
     *                  ; "from", (module Option(Time.Show))
     *                  ; "to", (module Option(Time.Show))
     *                  ; "duration", (module Option(Time.Relative)) ]
     *     control ids errors priority pids limit desc compress from till duration *)

    let get_percent ?(errors = []) ?(priority = []) ?(pids = []) ?(ids = [])
          ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / ("errors/percent" @/ empty))
        ~query:Query.[ "id", (module List(Stream.ID))
                     ; "errors", (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid", (module List(Int))
                     ; "from", (module Option(Time.Show))
                     ; "to", (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control ids errors priority pids from till duration

    let get_has_any ?(errors = []) ?(priority = []) ?(pids = []) ?(ids = [])
          ?from ?till ?duration control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / ("errors/has-any" @/ empty))
        ~query:Query.[ "id", (module List(Stream.ID))
                     ; "errors", (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid", (module List(Int))
                     ; "from", (module Option(Time.Show))
                     ; "to", (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control ids errors priority pids from till duration

  end

end
