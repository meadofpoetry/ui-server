open Netlib.Uri
open Pipeline_types
(*
module Api_websocket = Api_js.Websocket.Make(Body)

module Api_http = Api_js.Http.Make(Body)

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get ?f () =
    let t =
      Api_websocket.create
        ~path:Path.Format.("api/pipeline/settings" @/ empty)
        ~query:Query.empty () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Settings.of_yojson in
      t >>= fun socket ->
      Api_websocket.subscribe_map socket of_json @@ f socket;
      Lwt.return_ok socket

end

let set s =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("api/pipeline/settings" @/ empty)
    ~query:Query.empty
    ~body:(Settings.to_yojson s)
    (fun _env res -> Lwt.return res)

let get () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/settings" @/ empty)
    ~query:Query.empty
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Settings.of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)
 *)
