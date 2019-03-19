module type RO = sig

  type t

  type key = string

  type value = string

  type error = [
    | Futil.File.create_error
    | `Not_directory of string
    | `Bad_path of string
    ]
             
  type read_error = [
    | Futil.File.read_error
    | `Not_found
    ]
                          
  val pp_error : error Fmt.t

  val pp_read_error : read_error Fmt.t
    
  val create : path:string -> (t, [> error ]) result
    
  val read : t -> key list -> (value, [> read_error ]) Lwt_result.t

  val read_opt : t -> key list -> value option Lwt.t

end

type write_error = Futil.File.write_error

module type RW = sig
  
  include RO
     
  type watcher = value option -> value -> unit Lwt.t
               
  type write_error = Futil.File.write_error

  val pp_write_error : write_error Fmt.t

  val create : ?create:bool -> path:string -> (t, [> error ]) result
    
  val write : t -> key list -> value -> (unit, [> write_error ]) Lwt_result.t

  val watch : t -> key list -> watcher -> unit Lwt.t

  val unwatch : t -> key list -> unit Lwt.t
    
end
