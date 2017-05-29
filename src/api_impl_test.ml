open Containers

let test id meth args headers body =
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:(String.concat " " args) ()
