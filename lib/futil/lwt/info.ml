open Lwt.Infix

type error = Futil.Info.error
   
type t = Lwt_unix.stats

let pp_error = Futil.Info.pp_error
       
let info path =
  let path = Path.to_string path in
  Lwt.catch
    (fun () -> Lwt_unix.stat path >>= Lwt.return_ok)
    (function
     | Unix.Unix_error (Unix.ENOENT, _, _) ->
        Lwt.return_error `No_such_file
     | Unix.Unix_error (Unix.EACCES, _, _) ->
        Lwt.return_error `Access_denied
     | _ ->
        Lwt.return_error `Unspecified)

let exists path =
  let path = Path.to_string path in
  Lwt.catch
    (fun () ->
      Lwt_unix.access path [] >>= fun () ->
      Lwt.return_true)
    (fun _ -> Lwt.return_false)
      
let is_dir (stat : t) =
  stat.st_kind = Lwt_unix.S_DIR

let is_reg_file (stat : t) =
  stat.st_kind = Lwt_unix.S_REG

let is_socket (stat : t) =
  stat.st_kind = Lwt_unix.S_SOCK

let unix_perm (stat : t) =
  stat.st_perm
