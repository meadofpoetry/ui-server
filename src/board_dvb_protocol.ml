type resp  = Test of unit
type req = Resp of unit
  
let (init : req) = Resp ()

let (probes : req list) = [Resp ()]

let period = 5

let (serialize : req -> Board_meta.req_typ * Cbuffer.t) = fun _ -> `Instant, (Cbuffer.create 5)

let deserialize = fun _ -> [], None

let is_response = fun _ _ -> None

let is_free = fun _ -> None
