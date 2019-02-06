type key = string

type error = [
  | Futil.File.create_error
  | `Not_directory
  | `Bad_path of string
  ]

type write_error = Futil.File.write_error

type read_error = [
  | Futil.File.read_error
  | `Not_found
  ]
  
type t

val pp_error : error Fmt.t

val pp_write_error : write_error Fmt.t

val pp_read_error : read_error Fmt.t
   
val create : ?create:bool -> path:string -> (t, [> error ]) result
   
val read : t -> key list -> (string, [> read_error ]) Lwt_result.t

val read_opt : t -> key list -> string option Lwt.t

val write : t -> key list -> string -> (unit, [> write_error ]) Lwt_result.t
