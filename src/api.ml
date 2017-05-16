open Lwt
open Lwt_react
open Cohttp_lwt_unix

let handle = function
  | ["test"] -> Cohttp_lwt_unix.Server.respond_string
                  ~status:`OK
                  ~body:"test"
                  ()
  | _ ->  Cohttp_lwt_unix.Server.respond_not_found ()
