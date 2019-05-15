type t = Unix.stats

type error = [
  | Path.error
  | `No_such_file
  | `Access_denied
  | `Unspecified
  ]

let pp_error ppf = function
  | #Path.error as e -> Path.pp_error ppf e
  | `No_such_file -> Fmt.string ppf "no such file"
  | `Access_denied -> Fmt.string ppf "access denied"
  | `Unspecified -> Fmt.string ppf "UNSPECIFIED ERROR"
       
let info path =
  match Path.of_string path with
  | Error _ as e -> e
  | Ok path ->
     let path = Path.to_string path in
     try Ok (Unix.stat path) with
     | Unix.Unix_error (Unix.ENOENT, _, _) ->
        Error `No_such_file
     | Unix.Unix_error (Unix.EACCES, _, _) ->
        Error `Access_denied
     | _ ->
        Error `Unspecified

let exists path =
  match Path.of_string path with
  | Error _ -> false
  | Ok path ->
     let path = Path.to_string path in
     try Unix.access path []; true
     with _ -> false
      
let is_dir (stat : t) =
  stat.st_kind = Unix.S_DIR

let is_reg_file (stat : t) =
  stat.st_kind = Unix.S_REG

let is_socket (stat : t) =
  stat.st_kind = Unix.S_SOCK

let unix_perm (stat : t) =
  stat.st_perm
