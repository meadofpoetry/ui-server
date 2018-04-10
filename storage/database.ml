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
  type t = { db_path : string; cleanup : int } [@@deriving yojson]
  let default   = { db_path = "./db"; cleanup = 48 }
  let domain = "db"
end

module Conf = Config.Make(Settings)
              
module type MODEL = sig
  type _ req
  val name     : string
  val init     : (module Caqti_lwt.CONNECTION) -> unit Lwt.t
  val request  : (module Caqti_lwt.CONNECTION) -> 'a req -> 'a Lwt.t
  val cleanup  : (module Caqti_lwt.CONNECTION) -> unit Lwt.t
  val delete   : (module Caqti_lwt.CONNECTION) -> unit Lwt.t
  val worker   : ((module Caqti_lwt.CONNECTION) -> unit Lwt.t) option
end

type conf = { settings : Settings.t
            ; period   : float
            }
                  
module type CONN = sig
  type t
  type _ req
  val create   : conf -> (t, string) result
  val request  : t -> 'a req -> 'a Lwt.t
  val finalize : t -> unit
end

module Make (M : MODEL) : (CONN with type 'a req := 'a M.req) = struct
  type t = (module Caqti_lwt.CONNECTION)

  let create (conf : conf) =
    let path = Printf.sprintf "sqlite3:%s_%s?create=true&write=true" (realpath conf.settings.db_path) M.name in
    let db   = match Lwt_main.run @@ Caqti_lwt.connect (Uri.of_string path) with
      | Ok db   -> db
      | Error e -> failwith "Db connect failed with an error: %s\n" @@ Caqti_error.show e
    in
    let rec loop () =
      Lwt_unix.sleep conf.period >>= (fun () ->
      match M.worker with
      | None   -> Lwt.return_unit
      | Some w -> w db) >>= fun () ->
      M.cleanup db >>= loop
    in
    Lwt_main.run (M.init db);
    Lwt.async loop;
    Ok db

  let request (type a) db (req : a M.req) : a Lwt.t =
    M.request db req

  let finalize (module Db : Caqti_lwt.CONNECTION) =
    Lwt.async Db.disconnect
end

type t = conf
                                                              
let create config period =
  { settings = Conf.get config
  ; period
  }
