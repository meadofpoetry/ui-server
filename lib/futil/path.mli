type t

type error = [ `Bad_path of string ]

val pp_error : error Fmt.t

val of_string : string -> (t, [> error ]) result

val to_string : t -> string

val append : t -> string list -> t

val to_explicit_exn : string -> string
