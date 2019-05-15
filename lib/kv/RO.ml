type key = string

type value = string
               
module KMap = Map.Make(struct
                  type t = key list
                  let rec compare l r =
                    match l, r with
                    | [], [] -> 0
                    | [], _  -> -1
                    | _ , [] -> 1
                    | x::l', y::r' ->
                       let c = String.compare x y in
                       if c <> 0 then c
                       else compare l' r'
                end)
            
type t =
  { cached_lock : Lwt_mutex.t
  ; mutable cached : value KMap.t
  ; base_path : string
  }

type error = [
  | Futil.File.create_error
  | `Not_directory of string
  ]

type read_error = [
  | Futil.File.read_error
  | `Not_found of string
  ]

let pp_error ppf = function
  | #Futil.File.create_error as e -> Futil.File.pp_create_error ppf e
  | `Not_directory path -> Fmt.fmt "kv path %s refers not to a directory" ppf path

let pp_read_error ppf : read_error -> unit = function
  | #Futil.File.read_error as e -> Futil.File.pp_read_error ppf e
  | `Not_found s -> Fmt.fmt "file '%s' not found" ppf s
    
let (>>=) e f =
  match e with
  | Ok v -> f v
  | Error _ as er -> er

let ( / ) = Filename.concat

let cat = String.concat Filename.dir_sep
                   
let create ~path =
  let open Futil in
  match Path.of_string path with
  | Error _ as e -> e
  | Ok path' ->
     (if not (Info.exists path)
      then Error (`Not_directory path)
      else Ok ()) 
     >>= fun () ->
     Info.info path
     >>= fun info ->
     if not (Info.is_dir info)
     then Error (`Not_directory path)
     else if (Info.unix_perm info land 0o300) = 0
     then Error `Access_denied
     else
       let base_path = Futil.Path.to_string path' in
       Ok { cached_lock = Lwt_mutex.create ()
          ; cached = KMap.empty
          ; base_path
         }
     
let read kv keys =
  let open Futil_lwt in
  let (>>=) = Lwt.(>>=) in
  Lwt_mutex.with_lock kv.cached_lock
    (fun () -> Lwt.return @@ KMap.find_opt keys kv.cached)
  >>= function
  | Some v -> Lwt.return_ok v
  | None ->
     let path = kv.base_path / (cat keys) in
     File.read path
     >>= function
     | Error `No_such_file ->
        Lwt.return_error (`Not_found path)
     | Error _ as e ->
        Lwt.return e
     | Ok data' ->
        Lwt_mutex.with_lock kv.cached_lock
          (fun () ->
            Lwt.return (kv.cached <- KMap.update keys (fun _ -> Some data') kv.cached))
        >>= fun () ->
        Lwt.return_ok data'
        
let read_opt kv keys =
  let open Lwt.Infix in
  read kv keys >>= function
  | Ok v -> Lwt.return_some v
  | Error _ -> Lwt.return_none
