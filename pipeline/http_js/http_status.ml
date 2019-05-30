open Netlib.Uri
open Pipeline_types

module Api_http = Api_js.Http.Make(Body)

module Event = struct

  let get ?(ids = []) sock =
    let of_yojson = Util_json.List.of_yojson Qoe_status.of_yojson in
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("pipeline/status" @/ empty)
      ~query:Query.["id", (module List(Application_types.Stream.ID))]
      ids of_yojson sock

end

let get ?(ids = []) () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/status" @/ empty)
    ~query:Query.["id", (module List(Application_types.Stream.ID))]
    ids
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Util_json.List.of_yojson Qoe_status.of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)
