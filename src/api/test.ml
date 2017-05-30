open Containers

let test _ _ _ _ _ =
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"Thank you, master!" ()
