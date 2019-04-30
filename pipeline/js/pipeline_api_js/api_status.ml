open Api_common
open Netlib.Uri
open Application_types
open Pipeline_types

module Event = struct

  let ( >>= ) = Lwt_result.( >>= )

  let get ?on_error ?f ?(ids = []) () =
    let t =
      Api_websocket.create ?on_error
        ~path:Path.Format.("api/pipeline/status" @/ empty)
        ~query:Query.["id", (module List(Stream.ID))]
        ids () in
    match f with
    | None -> t
    | Some f ->
      let of_json = Qoe_status.status_list_of_yojson in
      t >>= fun socket ->
      Api_websocket.subscribe (f % map_ok of_json) socket;
      Lwt.return_ok socket

end

let get ?(ids = []) () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/pipeline/status" @/ empty)
    ~query:Query.["id", (module List(Stream.ID))]
    ids
    (fun _env -> function
       | Error e -> Lwt.return_error e
       | Ok x ->
         match Qoe_status.status_list_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok x -> Lwt.return_ok x)
