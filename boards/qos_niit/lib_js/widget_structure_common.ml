let base_class = "qos-niit-structure"

type dumpable =
  { name   : string * string
  ; get    : unit -> (string,string) Lwt_result.t
  ; prev   : string option React.signal
  }
