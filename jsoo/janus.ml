type js_obj = (string * Js.Unsafe.any) array

(* class type response = *)
(*   object *)

(*     (\* possible error info *\) *)
(*     method error      : Js.js_string Js.t Js.optdef_prop *)
(*     method error_code : Js.number Js.t Js.optdef_prop *)

(*     (\* possible streaming plugin fields *\) *)
(*     method list       : 'a array Js.optdef_prop *)
(*     method info       : 'b Js.t Js.optdef_prop *)
(*     method created    : 'c Js.t Js.optdef_prop *)
(*     method destroyed  : 'd Js.t Js.optdef_prop *)

(*     (\* mandatory (?) streaming plugin name containing string *\) *)
(*     method streaming   : Js.js_string Js.t Js.optdef_prop *)
(*   end *)

class type plugin =
  object
    method getId        : unit   -> Js.number Js.t Js.meth
    method getPlugin    : unit   -> Js.js_string Js.t Js.meth
    method send         : js_obj -> unit Js.meth
    method createAnswer : js_obj -> unit Js.meth
  end

class type janus = 
  object
    method getServer    : unit   -> Js.js_string Js.t Js.meth
    method isConnected  : unit   -> bool Js.t Js.meth
    method getSessionId : unit   -> Js.number Js.t Js.meth
    method attach       : js_obj -> unit Js.meth
    method destroy      : js_obj -> unit Js.meth
  end


let init data = Js.Unsafe.(fun_call (js_expr "Janus.init") [| obj data |]) |> ignore

let isWebrtcSupported () = Js.Unsafe.(fun_call (js_expr "Janus.isWebrtcSupported")) [||]

let attachMediaStream el_id stream =
  let el = Dom_html.document##getElementById (Js.string el_id) in
  Js.Unsafe.(fun_call (js_expr "Janus.attachMediaStream") [| inject el; inject stream|])
  
let create data = Js.Unsafe.(new_obj global##._Janus [| obj data |])
