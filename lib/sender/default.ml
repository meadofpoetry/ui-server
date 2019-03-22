open Types

type identity +=
   | Device of int * int   (* id * type *)
   | Stream of int32 (* stream_id *)

class device id typ = object
  inherit sender
  val tag             = Device (id, typ)
  val mutable is_sent = false
  method id         = tag
  method depends    = None
  method path       = "/var/devices/connect"
  method to_send    = is_sent
  method check = function
    | `OK -> is_sent <- true
    | _   -> is_sent <- false
  method get_data _ = Lwt.return_some (`Assoc [ ("serial", `Int id)
                                              ; ("type", `Int typ)
                                              ; ("name", `String "ATS")])
  method cleanup _  = Lwt.return_unit
end
(*
class stream id device =
  let Device (dev_id, _) = device in
  object
  inherit sender
  val tag             = Stream id
  val mutable is_sent = false
  method id         = tag
  method depends    = Some device
  method path       = "/var/streams/create"
  method to_send    = is_sent
  method check = function
    | `OK -> is_sent <- true
    | _   -> is_sent <- false
  method get_data _ = Lwt.return_some (`Assoc [ ("serial", `Int id)
                                              ; ("type", `Int typ)
                                              ; ("name", `String "ATS")])
  method cleanup _  = Lwt.return_unit
end

let make_default ident = ()
                         *)
