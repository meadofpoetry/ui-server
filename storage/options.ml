type path = string [@@deriving yojson]

type 'a storage = < get : 'a ; store : 'a -> unit >
                                               
module type CONFIG_STORAGE = sig
  type data

  val create : path -> path list -> data storage
end

module type DATA = sig
  type t
  val default : t
  val dump : t -> string
  val restore : string -> (t, string) result
end

module Make (D : DATA)
       : (CONFIG_STORAGE with type data = D.t) = struct

  open CCIO
  
  type data = D.t

  class storage path (current : data) = object
    val path = path
    val mutable current = current
    method get = current
    method store data =
       match File.write path (D.dump data) with
       | Error e -> failwith ("Config_storage create: something bad just happened: " ^ e)
       | Ok ()   -> current <- data
  end

  let create_dir d =
    try Unix.mkdir d 0o740; Ok d
    with Unix.Unix_error (Unix.EEXIST, _, d) -> Ok d
       | Unix.Unix_error (Unix.EACCES, _, d) -> Error ("Config_storage create:  no rights to work with " ^ d)
       | Unix.Unix_error (e, _, d) -> Error ("Config_storage create: " ^ (Unix.error_message e) ^ " " ^ d)
       | _ -> Error "Config_storage create: unknown error"      
            
  let create base path =
    let rec create' b = function
      | [] -> b
      | x::tl ->
         match create_dir (Filename.concat b x) with
         | Error e -> failwith e
         | Ok d    -> create' d tl
    in
    let path = Filename.concat (create' "" (base::path)) "data" in
    if File.exists path
    then match File.read path with
         | Error e -> failwith ("Config_storage create: something bad just happened: " ^ e)
         | Ok content ->
            match D.restore content with
            | Error e -> failwith ("Config_storage create: dump error: bad content " ^ e)
            | Ok cur  -> new storage path cur
    else match (File.write path (D.dump D.default)) with
         | Error e -> failwith ("Config_storage create: something bad just happened: " ^ e)
         | Ok ()   -> new storage path D.default

end

module Settings = struct

  type t = { config_dir : path } [@@deriving yojson]
  let default   = { config_dir = "/tmp" }
  let domain    = "config_storage"

end

module Conf = Config.Make(Settings)
