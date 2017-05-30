open Containers

let test _ _ args _ _ =
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:(String.concat " " args) ()
