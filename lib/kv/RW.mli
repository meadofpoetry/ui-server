include Intf.RW

val get : ?default:'a -> (value -> 'a) -> (value, [> read_error] as 'b) Lwt_result.t -> ('a, 'b) Lwt_result.t
