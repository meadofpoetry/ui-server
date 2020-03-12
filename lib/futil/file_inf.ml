module type S = sig
  type 'a io

  type create_error = [ Info.error | `File_already_exists | `No_space ]

  type write_error = [ create_error | `Writing_error of string ]

  type read_error = [ Info.error | `Reading_error of string ]

  type delete_error = [ Path.error | `Sys_error of string ]

  val pp_create_error : create_error Fmt.t

  val pp_write_error : write_error Fmt.t

  val pp_read_error : read_error Fmt.t

  val pp_delete_error : delete_error Fmt.t

  val default_mode : int

  val default_dir_mode : int

  val create_dir : ?dmode:int -> string -> (unit, [> create_error ]) result io

  val create :
    ?fmode:int -> ?dmode:int -> string -> (unit, [> create_error ]) result io

  val write :
    ?create:bool ->
    ?fmode:int ->
    ?dmode:int ->
    string ->
    string ->
    (unit, [> write_error ]) result io

  val read : string -> (string, [> read_error ]) result io

  val delete : string -> (unit, [> delete_error ]) result io
end
