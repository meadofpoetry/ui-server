module type S = sig

  type 'a io

  type t

  type error = [
    | `No_such_file
    | `Access_denied
    | `Unspecified
    ]

  val pp_error : error Fmt.t

  val info : Path.t -> (t, [> error ]) result io

  val exists : Path.t -> bool io

  val is_dir : t -> bool

  val is_reg_file : t -> bool

  val is_socket : t -> bool
    
  val unix_perm : t -> int

end
