open Containers
open Board_types
open Api_js.Requests.Json_request
open Common

let get_base_path () = Uri.Path.Format.(Boards_js.Requests.get_board_path () / ("errors" @/ empty))

module WS = struct

  open Common.Uri
  open Errors

  module TS = struct

    let get_errors ?(streams=[]) ?(errors=[]) ?(priority=[]) ?(pids=[]) control =
      let stream = List.map Stream.id_to_int32 streams in
      WS.get ~from:(Json.List.of_yojson of_yojson)
        ~path:Path.Format.(get_base_path () / ("ts" @/ empty))
        ~query:Query.[ "stream-id",(module List(Int32))
                     ; "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))]
        control stream errors priority pids

  end

  module T2MI = struct

    let get_errors ?(streams=[]) ?(t2mi_ids=[]) ?(errors=[]) ?(pids=[]) control =
      WS.get ~from:(Json.List.of_yojson of_yojson)
        ~path:Path.Format.(get_base_path () / ("t2mi" @/ empty))
        ~query:Query.[ "stream-id",      (module List(Int32))
                     ; "t2mi-stream-id", (module List(Int))
                     ; "errors",         (module List(Int))
                     ; "pid",            (module List(Int)) ]
        control streams t2mi_ids errors pids

  end

end

module HTTP = struct

  open Common.Uri
  open Errors

  module TS = struct

    module Archive = struct

      let get_errors ?(streams=[]) ?(errors=[]) ?(priority=[]) ?(pids=[])
            ?limit ?compress ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "not implemented")
          ~path:Path.Format.(get_base_path () / ("ts/archive" @/ empty))
          ~query:Query.[ "stream-id",(module List(Int32))
                       ; "errors",   (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid",      (module List(Int))
                       ; "limit",    (module Option(Int))
                       ; "compress", (module Option(Bool))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control streams errors priority pids limit compress from till duration

      let get_percent ?(streams=[]) ?(errors=[]) ?(priority=[]) ?(pids=[])
            ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "not implemented")
          ~path:Path.Format.(get_base_path () / ("ts/archive/percent" @/ empty))
          ~query:Query.[ "stream-id",(module List(Int32))
                       ; "errors",   (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid",      (module List(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control streams errors priority pids from till duration

      let get_has_any ?(streams=[]) ?(errors=[]) ?(priority=[]) ?(pids=[])
            ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "not implemented")
          ~path:Path.Format.(get_base_path () / ("ts/archive/has-any" @/ empty))
          ~query:Query.[ "stream-id", (module List(Int32))
                       ; "errors",   (module List(Int))
                       ; "priority", (module List(Int))
                       ; "pid",      (module List(Int))
                       ; "from",     (module Option(Time.Show))
                       ; "to",       (module Option(Time.Show))
                       ; "duration", (module Option(Time.Relative)) ]
          control streams errors priority pids from till duration

    end

  end

  module T2MI = struct

    module Archive = struct

      let get_errors ?(streams=[]) ?(t2mi_ids=[]) ?(errors=[]) ?(pids=[])
            ?limit ?compress ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "not implemented")
          ~path:Path.Format.(get_base_path () / ("t2mi/archive" @/ empty))
          ~query:Query.[ "stream-id",      (module List(Int32))
                       ; "t2mi-stream-id", (module List(Int))
                       ; "errors",         (module List(Int))
                       ; "pid",            (module List(Int))
                       ; "limit",          (module Option(Int))
                       ; "compress",       (module Option(Bool))
                       ; "from",           (module Option(Time.Show))
                       ; "to",             (module Option(Time.Show))
                       ; "duration",       (module Option(Time.Relative)) ]
          control streams t2mi_ids errors pids limit compress from till duration

      let get_percent ?(streams=[]) ?(t2mi_ids=[]) ?(errors=[]) ?(pids=[])
            ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "not implemented")
          ~path:Path.Format.(get_base_path () / ("t2mi/archive/percent" @/ empty))
          ~query:Query.[ "stream-id",      (module List(Int32))
                       ; "t2mi-stream-id", (module List(Int))
                       ; "errors",         (module List(Int))
                       ; "pid",            (module List(Int))
                       ; "limit",          (module Option(Int))
                       ; "compress",       (module Option(Bool))
                       ; "from",           (module Option(Time.Show))
                       ; "to",             (module Option(Time.Show))
                       ; "duration",       (module Option(Time.Relative)) ]
          control streams t2mi_ids errors pids from till duration

      let get_has_any ?(streams=[]) ?(t2mi_ids=[])  ?(errors=[]) ?(pids=[])
            ?from ?till ?duration control =
        get_result ~from:(fun _ -> Error "not implemented")
          ~path:Path.Format.(get_base_path () / ("t2mi/archive/has-any" @/ empty))
          ~query:Query.[ "stream-id",      (module List(Int32))
                       ; "t2mi-stream-id", (module List(Int))
                       ; "errors",         (module List(Int))
                       ; "pid",            (module List(Int))
                       ; "limit",          (module Option(Int))
                       ; "compress",       (module Option(Bool))
                       ; "from",           (module Option(Time.Show))
                       ; "to",             (module Option(Time.Show))
                       ; "duration",       (module Option(Time.Relative)) ]
          control streams t2mi_ids errors pids from till duration

    end

  end

end
