open Lwt.Infix

type create_error = Futil.File.create_error

type write_error = Futil.File.write_error

type read_error = Futil.File.read_error

type delete_error = Futil.File.delete_error

let pp_create_error = Futil.File.pp_create_error

let pp_write_error = Futil.File.pp_write_error

let pp_read_error = Futil.File.pp_read_error

let pp_delete_error = Futil.File.pp_delete_error

let default_mode = Futil.File.default_mode

let default_dir_mode = Futil.File.default_dir_mode

let () = assert (Filename.dir_sep = "/")

let exists path =
  Lwt.catch
    (fun () -> Lwt_unix.access path [] >>= fun () -> Lwt.return_true)
    (fun _ -> Lwt.return_false)

let create_single_dir mode d =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir d mode >>= fun () -> Lwt.return_ok ())
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_ok ()
      | Unix.Unix_error (Unix.EACCES, _, _) -> Lwt.return_error `Access_denied
      | Unix.Unix_error (Unix.ENOSPC, _, _) -> Lwt.return_error `No_space
      | _ -> Lwt.return_error `Unspecified)

let create_file mode f =
  Lwt.catch
    (fun () ->
      Lwt_unix.openfile f [ Unix.O_CREAT ] mode
      >>= Lwt_unix.close
      >>= Lwt.return_ok)
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
          Lwt.return_error `File_already_exists
      | Unix.Unix_error (Unix.EACCES, _, _) -> Lwt.return_error `Access_denied
      | Unix.Unix_error (Unix.ENOSPC, _, _) -> Lwt.return_error `No_space
      | _ -> Lwt.return_error `Unspecified)

let create_rec ?(dir = false) ?(dmode = default_dir_mode)
    ?(fmode = default_mode) path =
  let path_list =
    String.split_on_char '/' path |> List.filter (fun s -> String.length s <> 0)
    |> function
    | [] -> Error `Unspecified
    | "~" :: tl -> (
        try Ok (Unix.getenv "HOME" :: tl) with Not_found -> Error `Unspecified )
    | _ as p -> Ok ("/" :: p)
  in
  let rec create' base = function
    | [] -> Lwt.return_ok ()
    | [ x ] when not dir ->
        let path = Filename.concat base x in
        exists path >>= fun exists' ->
        if exists' then Lwt.return_ok () else create_file fmode path
    | h :: tl -> (
        let path = Filename.concat base h in
        exists path >>= fun exists' ->
        if exists' then create' path tl
        else
          create_single_dir dmode path >>= function
          | Ok () -> create' path tl
          | _ as e -> Lwt.return e )
  in
  match path_list with
  | Ok (base :: path) -> create' base path
  | Ok [] -> assert false (* Should be unreachable*)
  | Error _ as e -> Lwt.return e

let create_dir ?(dmode = default_dir_mode) path =
  match Path.of_string path with
  | Error _ as e -> Lwt.return e
  | Ok path' ->
      Info.exists path >>= fun exists ->
      if not exists then create_rec ~dmode (Path.to_string path')
      else Lwt.return_ok ()

let create ?(fmode = default_mode) ?(dmode = default_dir_mode) path =
  match Path.of_string path with
  | Error _ as e -> Lwt.return e
  | Ok path' ->
      Info.exists path >>= fun exists ->
      if not exists then create_rec ~fmode ~dmode (Path.to_string path')
      else Lwt.return_ok ()

let write ?(create = true) ?(fmode = default_mode) ?(dmode = default_dir_mode)
    path data =
  let ( >>=? ) = Lwt_result.bind in
  match Path.of_string path with
  | Error _ as e -> Lwt.return e
  | Ok path' ->
      let path_s = Path.to_string path' in
      Info.exists path >>= fun exists ->
      ( if create && not exists then create_rec ~fmode ~dmode path_s
      else Lwt.return_ok () )
      >>=? fun () ->
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Output path_s (fun oc ->
              Lwt_io.write oc data >>= Lwt.return_ok))
        (fun e -> Lwt.return_error (`Writing_error (Printexc.to_string e)))

let read path =
  match Path.of_string path with
  | Error _ as e -> Lwt.return e
  | Ok path' ->
      Info.exists path >>= fun exists ->
      if not exists then Lwt.return_error `No_such_file
      else
        let path_s = Path.to_string path' in
        Lwt.catch
          (fun () ->
            Lwt_io.with_file ~mode:Lwt_io.Input path_s (fun ic ->
                Lwt_io.read ic >>= Lwt.return_ok))
          (fun e -> Lwt.return_error (`Reading_error (Printexc.to_string e)))

let delete path =
  match Path.of_string path with
  | Error _ as e -> Lwt.return e
  | Ok path' ->
      Info.exists path >>= fun exists ->
      let path_s = Path.to_string path' in
      if not exists then
        Lwt.return_error (`Sys_error ("File.delete: file not exists " ^ path_s))
      else
        Lwt.catch
          (fun () -> Lwt_unix.unlink path_s >>= Lwt.return_ok)
          (fun _ ->
            Lwt.return_error
              (`Sys_error ("File.delete: unknown error " ^ path_s)))
