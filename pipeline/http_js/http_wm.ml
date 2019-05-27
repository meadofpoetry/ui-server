open Pipeline_types
open Netlib.Uri

module Api_websocket = Api_js.Websocket.Make(Body)

module Api_http = Api_js.Http.Make(Body)

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get ?f () =
    let t =
      Api_websocket.create
        ~path:Path.Format.("ws/pipeline/wm" @/ empty)
        ~query:Query.empty
        () in
    match f with
    | None -> t
    | Some f ->
      t >>= fun socket ->
      Api_websocket.subscribe_map socket Wm.of_yojson @@ f socket;
      Lwt.return_ok socket

end

let set_layout wm =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("api/pipeline/wm" @/ empty)
    ~query:Query.empty
    ~body:(Wm.to_yojson wm)
    (fun _env res -> Lwt.return res)

let get_layout () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/wm" @/ empty)
    ~query:Query.empty
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Wm.of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)
