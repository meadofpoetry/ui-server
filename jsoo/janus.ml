class type handle =
  object
    method getId  : Js.number Js.t Js.prop
  end

type parameters =
  { plugin    : string
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

type init_params =
  { debug    : bool Js.opt
  ; callback : (unit -> unit) Js.opt
  }
  
let init ?debug ?callback () =
  let opts = { debug = Js.Opt.option debug
             ; callback = Js.Opt.option callback
             } in
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "Janus.init") [|Js.Unsafe.inject opts|]

let create () = Js.Unsafe.new_obj Js.Unsafe.global##._Janus [||]
