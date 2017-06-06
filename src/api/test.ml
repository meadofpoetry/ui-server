open Containers

let (>>=) = Lwt.(>>=)

let test _ _ _ _ body =
  Lwt_io.printf "Test body: %s\n" body >>= fun _ ->
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"Thank you, master!" ()
