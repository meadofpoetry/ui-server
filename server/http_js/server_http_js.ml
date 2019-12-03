open Netlib.Uri
module Api_http = Api_js.Http.Make (Application_types.Body)

module Event = struct
  let get_config sock =
    let of_yojson = Server_types.settings_of_yojson in
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("server/config" @/ empty)
      ~query:Query.empty
      of_yojson
      sock
end

let get_config () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/server/config" @/ empty)
    ~query:Query.empty
    (fun _env -> function
      | Error _ as e -> Lwt.return e
      | Ok x -> (
        match Server_types.settings_of_yojson x with
        | Error e -> Lwt.return_error (`Msg e)
        | Ok _ as x -> Lwt.return x))

let restart () =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("api/server/restart" @/ empty)
    ~query:Query.empty
    (fun _env x -> Lwt.return x)

let set_https_enabled (x : bool) =
  Api_http.perform_unit
    ~meth:`POST
    ~body:(`Bool x)
    ~path:Path.Format.("api/server/config/https-enabled" @/ empty)
    ~query:Query.empty
    (fun _env -> Lwt.return)

let set_tls_crt
    ?upload_progress
    ~(name : string)
    (cert : Js_of_ocaml.File.blob Js_of_ocaml.Js.t) =
  Api_js.Http.perform_file
    ?upload_progress
    ~file:cert
    ~path:Path.Format.("api/server/config/crt" @/ String ^/ empty)
    ~query:Query.empty
    name
    (fun _env x -> Lwt.return x)

let set_tls_key
    ?upload_progress
    ~(name : string)
    (key : Js_of_ocaml.File.blob Js_of_ocaml.Js.t) =
  Api_js.Http.perform_file
    ?upload_progress
    ~file:key
    ~path:Path.Format.("api/server/config/key" @/ String ^/ empty)
    ~query:Query.empty
    name
    (fun _env x -> Lwt.return x)

let delete_tls_crt () =
  Api_http.perform_unit
    ~meth:`DELETE
    ~path:Path.Format.("api/server/config/crt" @/ empty)
    ~query:Query.empty
    (fun _env x -> Lwt.return x)

let delete_tls_key () =
  Api_http.perform_unit
    ~meth:`DELETE
    ~path:Path.Format.("api/server/config/key" @/ empty)
    ~query:Query.empty
    (fun _env x -> Lwt.return x)
