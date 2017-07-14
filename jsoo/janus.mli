type js_obj = (string * Js.Unsafe.any) array

class type plugin =
  object
    method getId        : unit   -> Js.number Js.t Js.meth
    method getPlugin    : unit   -> Js.js_string Js.t Js.meth
    method send         : js_obj -> unit Js.meth
    method createAnswer : js_obj -> unit Js.meth
  end

class type janus = 
  object
    method getServer     : unit   -> Js.js_string Js.t Js.meth
    method isConnected   : unit   -> bool Js.t Js.meth
    method getSessionId  : unit   -> Js.number Js.t Js.meth
    method attach        : js_obj -> unit Js.meth
    method destroy       : js_obj -> unit Js.meth
  end

(* Janus initialization *)
val init : js_obj -> unit

(* Create Janus session *)
val create : js_obj -> 'a

(* Helper method to check whether WebRTC is supported by the browser *)
val isWebrtcSupported : unit -> bool

(* Helper method to attach a stream to a html video element *)
val attachMediaStream : string -> Js.json Js.t -> unit
