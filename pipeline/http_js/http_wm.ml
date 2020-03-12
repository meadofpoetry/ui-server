open Pipeline_types
open Netlib.Uri
module Api_http = Api_js.Http.Make (Body)

module Event = struct
  let get sock =
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("pipeline/wm" @/ empty)
      ~query:Query.empty Wm.Annotated.of_yojson sock
end

let set_layout wm =
  Api_http.perform_unit ~meth:`POST
    ~path:Path.Format.("api/pipeline/wm" @/ empty)
    ~query:Query.empty ~body:(Wm.to_yojson wm)
    (fun _env res -> Lwt.return res)

let get_layout () =
  Api_http.perform ~meth:`GET
    ~path:Path.Format.("api/pipeline/wm" @/ empty)
    ~query:Query.empty
    (fun _env -> function Error e -> Lwt.return_error e
      | Ok x -> (
          match Wm.Annotated.of_yojson x with
          | Error e -> Lwt.return_error (`Msg e)
          | Ok x -> Lwt.return_ok x ))
