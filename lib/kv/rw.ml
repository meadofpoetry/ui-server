type error = [
  | Futil.File.create_error
  | `Not_directory
  | `Bad_path of string
  ]

type write_error = Futil.File.write_error

type read_error = [
  | Futil.File.read_error
  | `Not_found
  | `Parse_error of string
  ]

module type S = sig
                
  type t

  type key = string

  type value
     
  type watcher = value option -> value -> unit Lwt.t

  type error = private [>
    | Futil.File.create_error
    | `Not_directory
    | `Bad_path of string
    ]

  type write_error = private [> Futil.File.write_error ]

  type read_error = private [>
    | Futil.File.read_error
    | `Not_found
    | `Parse_error of string
    ]    
                  
  val pp_error : error Fmt.t

  val pp_write_error : write_error Fmt.t

  val pp_read_error : read_error Fmt.t
   
  val create : ?create:bool -> path:string -> (t, error) result
   
  val read : t -> key list -> (value, read_error) Lwt_result.t

  val read_opt : t -> key list -> value option Lwt.t

  val write : t -> key list -> value -> (unit, write_error) Lwt_result.t

  val watch : t -> key list -> watcher -> unit Lwt.t

  val unwatch : t -> key list -> unit Lwt.t

end

module Make (V : Value.S) : S with type value = V.t = struct

  type key = string

  type value = V.t

  type watcher = value option -> value -> unit Lwt.t
               
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
    ; watchers_lock : Lwt_mutex.t
    ; mutable watchers : watcher KMap.t
    ; base_path : Futil.Path.t
    }

  type error = [
    | Futil.File.create_error
    | `Not_directory
    | `Bad_path of string
    ]

  type write_error = Futil.File.write_error

  type read_error = [
    | Futil.File.read_error
    | `Not_found
    | `Parse_error of string
    ]

  let pp_error ppf = function
    | #Futil.File.create_error as e -> Futil.File.pp_create_error ppf e
    | `Bad_path path -> Fmt.fmt "bad path: %s" ppf path
    | `Not_directory -> Fmt.string ppf "kv path refers not to a directory"

  let pp_write_error = Futil.File.pp_write_error

  let pp_read_error ppf = function
    | #Futil.File.read_error as e -> Futil.File.pp_read_error ppf e
    | `Not_found -> Fmt.string ppf "not found"
    | `Parse_error msg -> Fmt.fmt "value parse error: %s" ppf msg

  let parse s =
    try Lwt.return_ok @@ V.of_string s
    with e -> Lwt.return_error (`Parse_error (Printexc.to_string e))
                  
  let (>>=) e f =
    match e with
    | Ok v -> f v
    | Error _ as er -> er

  let create ?(create = false) ~path =
    let open Futil in
    match Path.of_string path with
    | Error e -> Error (`Bad_path e)
    | Ok path' ->
       (if not (Info.exists path') && create
        then File.create_dir path'
        else Ok ())
       >>= fun () ->
       Info.info path'
       >>= fun info ->
       if not (Info.is_dir info)
       then Error `Not_directory
       else if (Info.unix_perm info land 0o300) = 0
       then Error `Access_denied
       else Ok { cached_lock = Lwt_mutex.create ()
               ; cached = KMap.empty
               ; watchers_lock = Lwt_mutex.create ()
               ; watchers = KMap.empty
               ; base_path = path'
              }
       
  let read kv keys =
    let open Futil_lwt in
    let (>>=) = Lwt.(>>=) in
    let (>>=?) = Lwt_result.(>>=) in
    Lwt_mutex.with_lock kv.cached_lock
      (fun () -> Lwt.return @@ KMap.find_opt keys kv.cached)
    >>= function
    | Some v -> Lwt.return_ok v
    | None ->
       let path = Path.append kv.base_path keys in
       File.read path
       >>= function
       | Error `No_such_file ->
          Lwt.return_error `Not_found
       | Error _ as e ->
          Lwt.return e
       | Ok data ->
          parse data
          >>=? fun data' ->
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

  let write kv keys data =
    let open Lwt.Infix in
    let open Futil_lwt in
    Lwt_mutex.with_lock kv.cached_lock
      (fun () -> Lwt.return @@ KMap.find_opt keys kv.cached)
    >>= function
    | Some old when V.equal data old ->
       Lwt.return_ok ()
    | _ as old ->
       let open Lwt_result.Infix in
       let path = Path.append kv.base_path keys in
       File.write path (V.to_string data)
       >>= fun () ->
       let open Lwt.Infix in
       Lwt_mutex.with_lock kv.cached_lock
         (fun () -> Lwt.return @@
                      kv.cached <- KMap.update keys (fun _ -> Some data) kv.cached)
       >>= fun () ->
       Lwt_mutex.with_lock kv.watchers_lock
         (fun () -> Lwt.return @@ KMap.find_opt keys kv.watchers)
       >>= function
       | None   -> Lwt.return_ok ()
       | Some w ->
          let open Lwt.Infix in
          w old data
          >>= Lwt.return_ok
          
  let watch kv keys watcher =
    Lwt_mutex.with_lock kv.watchers_lock
      (fun _ -> Lwt.return @@
                  kv.watchers <- KMap.update keys (fun _ -> Some watcher) kv.watchers)

  let unwatch kv keys =
    Lwt_mutex.with_lock kv.watchers_lock
      (fun _ -> Lwt.return @@
                  kv.watchers <- KMap.remove keys kv.watchers)

end
