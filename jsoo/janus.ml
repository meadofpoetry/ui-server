let (>|=) x f = Js.Opt.map x f

class type handle =
  object
    method getId  : Js.number Js.t Js.prop
  end
  
type parameters =
  { plugin    : Js.js_string Js.opt
  ; success   : handle Js.t -> unit
  }

class type janus = 
  object
    method getServer     : Js.js_string Js.t Js.prop
    method isConnected   : bool Js.t Js.prop
    method getSessionId  : Js.number Js.t Js.prop
    method attach        : parameters -> unit Js.meth
    method destroy       : parameters -> unit Js.meth
  end

  
let init ?debug ?callback () =
  let debug = Js.Opt.option debug in
  let cb    = (Js.Opt.option callback) >|= Js.wrap_callback in
  Js.Unsafe.(
    fun_call (Js.Unsafe.js_expr "Janus.init") [|obj [|("debug", inject debug);
                                                      ("callback", inject cb)|] |]
  )

let isWebrtcSupported () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "Janus.isWebrtcSupported") [||]
  
let create () = Js.Unsafe.new_obj Js.Unsafe.global##._Janus [||]
