open Pipeline_types
open Netlib.Uri
module Api_http = Api_js.Http.Make (Body)

module Event = struct
  let get_annotated sock =
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("pipeline/structures/annotated" @/ empty)
      ~query:Query.empty
      Structure.Annotated.of_yojson
      sock

  let get_applied_structures ?(inputs = []) ?(ids = []) sock =
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("pipeline/structures/applied" @/ empty)
      ~query:
        Query.
          [ "id", (module List (Application_types.Stream.ID))
          ; "input", (module List (Application_types.Topology.Show_topo_input)) ]
      ids
      inputs
      Structure.Many.of_yojson
      sock
end

let get_annotated () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/structures/annotated" @/ empty)
    ~query:Query.empty
    (fun _env -> function
      | Error e -> Lwt.return_error e
      | Ok x -> (
        match Structure.Annotated.of_yojson x with
        | Error e -> Lwt.return_error (`Msg e)
        | Ok x -> Lwt.return_ok x))

let get_applied_structures ?(inputs = []) ?(ids = []) () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/structures/applied" @/ empty)
    ~query:
      Query.
        [ "id", (module List (Application_types.Stream.ID))
        ; "input", (module List (Application_types.Topology.Show_topo_input)) ]
    ids
    inputs
    (fun _env -> function
      | Error e -> Lwt.return_error e
      | Ok x -> (
        match Structure.Many.of_yojson x with
        | Error e -> Lwt.return_error (`Msg e)
        | Ok x -> Lwt.return_ok x))

let apply_structures s =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("api/pipeline/structures/apply" @/ empty)
    ~query:Query.empty
    ~body:(Structure.Many.to_yojson s)
    (fun _env res -> Lwt.return res)
