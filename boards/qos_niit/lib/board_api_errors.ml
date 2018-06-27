open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Api.Redirect
open Common

type events = errors_events

module WS = struct

  open Errors

  let rec filter (e:t) = function
    | []    -> true
    | f::tl -> if not @@ f e then false else filter e tl

  module TS = struct

    let errors (events:events) stream errors priority pids _ body sock_data () =
      let stream = Option.map Stream.id_of_int32 stream in
      let f_stream   = Option.map (fun id -> fun e -> Stream.equal_id id e.stream) stream in
      let f_errors   = match errors with [] -> None | l  -> Some (fun e -> List.mem ~eq:(=) e.err_code l) in
      let f_priority = match priority with [] -> None | l -> Some (fun e -> List.mem ~eq:(=) e.err_code l) in
      let f_pids     = match pids with [] -> None | l -> Some (fun e -> List.mem ~eq:(=) e.pid l) in
      let fns        = List.filter_map (fun x -> x) [f_stream;f_errors;f_priority;f_pids] in
      let e          = React.E.fmap (fun l -> match List.filter (fun (e:t) -> filter e fns) l with
                                              | [] -> None
                                              | l  -> Some l) events.ts_errors
      in sock_handler sock_data e (Json.list_to_yojson to_yojson) body

  end

  module T2MI = struct

    let errors (events:events) stream t2mi_id errors pids _ body sock_data () =
      let stream = Option.map Stream.id_of_int32 stream in
      let f_stream  = Option.map (fun id -> fun e -> Stream.equal_id id e.stream) stream in
      let f_t2mi_id = Option.map (fun id -> fun e -> Int.equal id (Int32.to_int e.param_2)) t2mi_id in
      let f_errors   = match errors with [] -> None | l  -> Some (fun e -> List.mem ~eq:(=) e.err_code l) in
      let f_pids     = match pids with [] -> None | l -> Some (fun e -> List.mem ~eq:(=) e.pid l) in
      let fns       = List.filter_map (fun x -> x) [f_stream;f_t2mi_id;f_errors;f_pids] in
      let e = React.E.fmap (fun l -> match List.filter (fun (e:t) -> filter e fns) l with
                                     | [] -> None
                                     | l  -> Some l) events.t2mi_errors
      in sock_handler sock_data e (Json.list_to_yojson to_yojson) body

  end

end

module HTTP = struct

  module TS = struct

    module Archive = struct

      let errors stream errors priority pids limit compress from till duration _ _ () =
        respond_error ~status:`Not_implemented "not implemented" ()
      let percent stream errors priority pids from till duration _ _ () =
        respond_error ~status:`Not_implemented "not implemented" ()
      let has_any stream errors priority pids from till duration _ _() =
        respond_error ~status:`Not_implemented "not implemented" ()

    end
  end

  module T2MI = struct

    module Archive = struct

      let errors stream t2mi_id errors pids limit compress from till duration _ _ () =
        respond_error ~status:`Not_implemented "not implemented" ()
      let percent stream t2mi_id errors pids from till duration _ _ () =
        respond_error ~status:`Not_implemented "not implemented" ()
      let has_any stream t2mi_id errors pids from till duration _ _() =
        respond_error ~status:`Not_implemented "not implemented" ()

    end

  end
end

let ts_handler events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "ts"
    [ create_ws_handler ~docstring:"Pushes TS errors to the client"
                        ~path:Path.Format.empty
                        ~query:Query.[ "stream",   (module Option(Int32))
                                     ; "errors",   (module List(Int))
                                     ; "priority", (module List(Int))
                                     ; "pid",      (module List(Int))]
                        (WS.TS.errors events)
    ]
    [ `GET, [ create_handler ~docstring:"Returns TS errors archive"
                             ~path:Path.Format.("archive" @/ empty)
                             ~query:Query.[ "stream",   (module Option(Int32))
                                          ; "errors",   (module List(Int))
                                          ; "priority", (module List(Int))
                                          ; "pid",      (module List(Int))
                                          ; "limit",    (module Option(Int))
                                          ; "compress", (module Option(Bool))
                                          ; "from",     (module Option(Time.Show))
                                          ; "to",       (module Option(Time.Show))
                                          ; "duration", (module Option(Time.Relative)) ]
                             (HTTP.TS.Archive.errors)
            ; create_handler ~docstring:"Returns TS errors presence percentage"
                             ~path:Path.Format.("archive/percent" @/ empty)
                             ~query:Query.[ "stream",   (module Option(Int32))
                                          ; "errors",   (module List(Int))
                                          ; "priority", (module List(Int))
                                          ; "pid",      (module List(Int))
                                          ; "from",     (module Option(Time.Show))
                                          ; "to",       (module Option(Time.Show))
                                          ; "duration", (module Option(Time.Relative)) ]
                             (HTTP.TS.Archive.percent)
            ; create_handler ~docstring:"Returns if TS errors were present for the requested period"
                             ~path:Path.Format.("archive/has-any" @/ empty)
                             ~query:Query.[ "stream",   (module Option(Int32))
                                          ; "errors",   (module List(Int))
                                          ; "priority", (module List(Int))
                                          ; "pid",      (module List(Int))
                                          ; "from",     (module Option(Time.Show))
                                          ; "to",       (module Option(Time.Show))
                                          ; "duration", (module Option(Time.Relative)) ]
                             (HTTP.TS.Archive.has_any)
            ]
    ]

let t2mi_handler events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "t2mi"
    [ create_ws_handler ~docstring:"Pushes T2-MI errors to the client"
                        ~path:Path.Format.empty
                        ~query:Query.[ "stream",         (module Option(Int32))
                                     ; "t2mi-stream-id", (module Option(Int))
                                     ; "errors",         (module List(Int))
                                     ; "pid",            (module List(Int)) ]
                        (WS.T2MI.errors events)
    ]
    [ `GET, [ create_handler ~docstring:"Returns T2-MI errors archive"
                             ~path:Path.Format.("archive" @/ empty)
                             ~query:Query.[ "stream",         (module Option(Int32))
                                          ; "t2mi-stream-id", (module Option(Int))
                                          ; "errors",         (module List(Int))
                                          ; "pid",            (module List(Int))
                                          ; "limit",          (module Option(Int))
                                          ; "compress",       (module Option(Bool))
                                          ; "from",           (module Option(Time.Show))
                                          ; "to",             (module Option(Time.Show))
                                          ; "duration",       (module Option(Time.Relative)) ]
                             (HTTP.T2MI.Archive.errors)
            ; create_handler ~docstring:"Returns T2-MI errors presense percentage"
                             ~path:Path.Format.("archive/percent" @/ empty)
                             ~query:Query.[ "stream",         (module Option(Int32))
                                          ; "t2mi-stream-id", (module Option(Int))
                                          ; "errors",         (module List(Int))
                                          ; "pid",            (module List(Int))
                                          ; "from",           (module Option(Time.Show))
                                          ; "to",             (module Option(Time.Show))
                                          ; "duration",       (module Option(Time.Relative)) ]
                             (HTTP.T2MI.Archive.percent)
            ; create_handler ~docstring:"Returns if T2-MI errors were present for the requested period"
                             ~path:Path.Format.("archive/has-any" @/ empty)
                             ~query:Query.[ "stream",         (module Option(Int32))
                                          ; "t2mi-stream-id", (module Option(Int))
                                          ; "errors",         (module List(Int))
                                          ; "pid",            (module List(Int))
                                          ; "from",           (module Option(Time.Show))
                                          ; "to",             (module Option(Time.Show))
                                          ; "duration",       (module Option(Time.Relative)) ]
                             (HTTP.T2MI.Archive.has_any) ]
    ]

let handlers events =
  [ ts_handler events
  ; t2mi_handler events
  ]
