let ( % ) f g x = f (g x)

let map_err = function
  | Error e -> Error (`Conv_error e)
  | Ok _ as x -> x

let ignore_env = fun _ x -> Lwt.return x

let ignore_env_bind f = fun _ -> function
  | Error _ as e -> Lwt.return e
  | Ok x -> f x

module Api_http = Api_js.Http.Make(Application_types.Body)
