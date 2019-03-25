open React

val dispatch : Yojson.Safe.json -> unit

val add_event : name:string
                -> (Yojson.Safe.json -> ('a, string) result)
                -> 'a event

val add_signal : name:string
                 -> eq:('a -> 'a -> bool)
                 -> init:'a
                 -> (Yojson.Safe.json -> ('a, string) result)
                 -> 'a signal
