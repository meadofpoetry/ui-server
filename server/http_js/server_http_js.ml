open Netlib.Uri

module Api_http = Api_js.Http.Make(Application_types.Body)

let get_config () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/server/config" @/ empty)
    ~query:Query.empty
    (fun _env -> function
       | Error _ as e -> Lwt.return e
       | Ok x ->
         match Server_types.settings_of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok _ as x -> Lwt.return x)

let restart () =
  Api_http.perform_unit
    ~meth:`POST
    ~path:Path.Format.("api/server/restart" @/ empty)
    ~query:Query.empty
    (fun _env x -> Lwt.return x)

let set_tls_crt
    ~(name : string)
    (cert : Js_of_ocaml.File.blob Js_of_ocaml.Js.t) =
  Api_js.Http.perform_file
    ~file:cert
    ~path:Path.Format.("api/server/config/crt" @/ String ^/ empty)
    ~query:Query.empty
    name
    (fun _env x -> Lwt.return x)

let set_tls_key
    ~(name : string)
    (key : Js_of_ocaml.File.blob Js_of_ocaml.Js.t) =
  Api_js.Http.perform_file
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
