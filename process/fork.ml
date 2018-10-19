open Lwt.Infix

let fork : type a. (unit -> unit) -> Unix.process_status Lwt.t = fun child ->
  Lwt_io.flush_all () >>= fun () ->
  match Lwt_unix.fork () with
  | n when n < 0 -> Lwt.fail_with "fork failure"
  | 0            -> child (); exit 0
  | pid          ->
     Lwt_unix.waitpid [] pid >>= fun (_, status) ->
     Lwt.return status
