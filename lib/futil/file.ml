type create_error = [
    Info.error
  | `File_already_exists
  | `No_space
  ]

type write_error = [
    create_error
  | `Writing_error of string 
  ]

type read_error = [
    Info.error
  | `Reading_error of string
  ]

let pp_create_error ppf = function
  | #Info.error as e -> Info.pp_error ppf e
  | `File_already_exists -> Fmt.string ppf "file already exists"
  | `No_space -> Fmt.string ppf "no space left on the device"

let pp_write_error ppf = function
  | #create_error as e -> pp_create_error ppf e
  | `Writing_error e -> Fmt.fmt "writing error: %s" ppf e

let pp_read_error ppf = function
  | #Info.error as e -> Info.pp_error ppf e
  | `Reading_error e -> Fmt.fmt "reading error: %s" ppf e

let default_mode = 0o644

let default_dir_mode = 0o755

let page_size = 65536
                 
let () = assert (Filename.dir_sep = "/")

let finalizer res f ~fin =
  try let r = f res in
      fin res;
      r
  with e ->
    fin res;
    raise e
       
let (>>=) e f = match e with
  | Ok v -> f v
  | Error _ as e -> e

let exists path =
  try Unix.access path []; true
  with _ -> false
                  
let create_single_dir mode d =
  try Unix.mkdir d mode; Ok ()
  with Unix.Unix_error (Unix.EEXIST, _, _) ->
        Ok ()
     | Unix.Unix_error (Unix.EACCES, _, _) ->
        Error `Access_denied
     | Unix.Unix_error (Unix.ENOSPC, _, _) ->
        Error `No_space
     | _ ->
        Error `Unspecified

let create_file mode f =
  try Unix.openfile f [Unix.O_CREAT] mode
      |> Unix.close;
      Ok ()
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) ->
     Error `File_already_exists
  | Unix.Unix_error (Unix.EACCES, _, _) ->
     Error `Access_denied
  | Unix.Unix_error (Unix.ENOSPC, _, _) ->
     Error `No_space
  | _ ->
     Error `Unspecified
        
let create_rec
      ?(dir = false)
      ?(dmode = default_dir_mode)
      ?(fmode = default_mode)
      path =
  let path_list =
    String.split_on_char '/' path
    |> List.filter (fun s -> String.length s <> 0)
    |> function [] -> Error `Unspecified
              | "~"::tl -> begin try
                               Ok ((Unix.getenv "HOME")::tl)
                             with Not_found -> Error `Unspecified
                           end
              | _ as p -> Ok ("/"::p)
  in
  let rec create' base = function
    | [] -> Ok ()
    | [x] when not dir ->
       let path = Filename.concat base x in
       if exists path
       then Ok ()
       else create_file fmode path
    | h::tl ->
       let path = Filename.concat base h in
       if exists path
       then create' path tl
       else begin
           create_single_dir dmode path
           >>= fun _ ->
           create' path tl
         end
  in
  match path_list with
  | Ok (base::path) -> create' base path
  | Ok [] -> assert false (* Should be unreachable*)
  | Error _ as e -> e

let create_dir ?(dmode = default_dir_mode) path =
  if not (Info.exists path)
  then create_rec ~dir:true ~dmode (Path.to_string path)
  else Ok ()
                 
let create ?(fmode = default_mode) ?(dmode = default_dir_mode) path =
  if not (Info.exists path)
  then create_rec ~dir:false ~fmode ~dmode (Path.to_string path)
  else Ok ()
                 
let write ?(create = true) ?(fmode = default_mode) ?(dmode = default_dir_mode) path data =
  let path_s = Path.to_string path in
  (if create && not (Info.exists path)
   then create_rec ~fmode ~dmode path_s
   else Ok ())
  >>= fun () ->
  try
    let oc = open_out_bin path_s in
    finalizer oc
      (fun oc -> output_string oc data)
      ~fin:close_out;
    Ok ()
  with Sys_error e -> Error (`Writing_error e)

let read path =
  let path_s = Path.to_string path in
  if not (Info.exists path)
  then Error `No_such_file
  else
    try
      let ic = open_in_bin path_s in
      let buf = ref (Bytes.create page_size) in
      let len = ref 0 in
      finalizer ic
        ~fin:close_in
        (fun ic -> try
            while true do
              (* resize *)
              if !len = Bytes.length !buf then (
                buf := Bytes.extend !buf 0 !len;
              );
              assert (Bytes.length !buf > !len);
              let n = input ic !buf !len (Bytes.length !buf - !len) in
              len := !len + n;
              if n = 0 then raise Exit;  (* exhausted *)
            done;
            assert false (* never reached*)
          with Exit -> Ok (Bytes.sub_string !buf 0 !len))
    with Sys_error e -> Error (`Reading_error e)
       | Out_of_memory -> Error (`Reading_error "out of memory")

let delete path =
  try Ok (Sys.remove (Path.to_string path))
  with Sys_error e -> Error e
