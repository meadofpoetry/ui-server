type action =
  { callback : unit -> unit
  ; name : string
  ; icon : string
  ; context : context
  }
and context = All | Selected | Empty
