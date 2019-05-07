open Netlib.Uri
open Pipeline_types

module Api_websocket = Api_js.Websocket.Make(Body)

module Api_http = Api_js.Http.Make(Body)

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get ?f ?(ids = []) () =
    let t =
      Api_websocket.create
        ~path:Path.Format.("api/pipeline/status" @/ empty)
        ~query:Query.["id", (module List(Application_types.Stream.ID))]
        ids () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Util_json.List.of_yojson Qoe_status.of_yojson in
      t >>= fun socket ->
      Api_websocket.subscribe_map socket of_json @@ f socket;
      Lwt.return_ok socket

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
