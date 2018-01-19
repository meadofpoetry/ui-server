type t

val empty : t
               
val of_string : string -> t

val of_string_list : string list -> t

val to_string : t -> string

val to_string_list : t -> string list

val is_empty : t -> bool
                               
val is_absolute : t -> bool

val is_absolute_ref : t -> bool

val make_absolute : t -> t

val make_absolute_ref : t -> t
          
val concat : t list -> t
