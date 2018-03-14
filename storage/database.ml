open Containers
open Lwt.Infix

(* TODO fix-it *)
let realpath path =
  if not @@ Filename.is_relative path
  then path
  else begin
      let top::rest = String.split ~by:Filename.dir_sep path in
      let top = match top with
        | "~"  -> Sys.getenv "HOME"
        | "."  -> Unix.getcwd ()
        | ".." -> Unix.getcwd () ^ ".."
        | top  -> top
      in
      let rest = List.filter_map (function
                     | "." -> None
                     | str when String.prefix str "$" ->
                        Option.(String.chop_prefix ~pre:"$" str >>= Sys.getenv_opt)
                     | str -> Some str)
                   rest
      in
      String.concat Filename.dir_sep (top::rest)
    end
  
module Settings = struct
  type t = { db_path : string } [@@deriving yojson]
  let default   = { db_path = "./db" }
  let domain = "db"
end

module Conf = Config.Make(Settings)
            
type t        = { db      : (module Caqti_lwt.CONNECTION)
                ; mutex   : Lwt_mutex.t
                ; workers : (t -> unit Lwt.t) list ref
                }

module type STORAGE = sig
  type _ req
  val init     : t -> unit Lwt.t
  val request  : t -> 'a req -> 'a
end
                    
let create config period =
  let cfg  = Conf.get config in
  let path = Printf.sprintf "sqlite3:%s?create=true&write=true" (realpath cfg.db_path) in
  let db   = match Lwt_main.run @@ Caqti_lwt.connect (Uri.of_string path) with
    | Ok db   -> db
    | Error e -> failwith "Db connect failed with an error: %s\n" @@ Caqti_error.show e
  in
  let workers = ref [] in
  let obj = { db; mutex = Lwt_mutex.create (); workers } in
  let rec loop () =
    let rec traverse = function
      | [] -> Lwt.return_unit
      | x::tl -> x obj >>= fun () -> traverse tl
    in
    Lwt_unix.sleep period >>= fun () ->
    traverse !workers     >>= loop
  in
  obj, loop ()

let with_connection (type a) o (f : (module Caqti_lwt.CONNECTION) -> a Lwt.t) : a Lwt.t =
  Lwt_mutex.with_lock o.mutex (fun () -> f o.db)

let add_maintainer o f =
  o.workers := f :: !(o.workers)

let finalize o =
  let (module Db : Caqti_lwt.CONNECTION) = o.db in
  print_endline "closing db";
  Lwt.ignore_result @@ Db.disconnect ()
