open Board_types.Streams.TS

let base_class = "qos-niit-structure"

type dumpable =
  { name : string * string
  ; get  : unit -> (section,string) Lwt_result.t
  ; prev : section option ref
  }
