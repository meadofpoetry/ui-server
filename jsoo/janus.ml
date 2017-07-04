let (>|=) x f = Js.Opt.map x f

class type handle =
  object
    method getId  : Js.number Js.t Js.prop
  end
  
type parameters

let make_parameters ?plugin
                    ?success
                    () =
  let open Js.Unsafe in
  let plugin' = ("plugin", Js.Opt.option plugin >|= Js.string |> inject) in
  let success' = ("success", Js.Opt.option success >|= Js.wrap_callback |> inject) in
  obj [|plugin'; success'|]
    
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
  
let create ~server
           ?iceServers 
           ?ipv6             
           ?withCredentials    
           ?max_poll_events     
           ?destroyOnUnload   
           ?token           
           ?apisecret     
           ?success      
           ?error
           ?destroyed
           () =
  let open Js.Unsafe in
  let server'          = ("server",          Js.string server |> inject) in
  let iceServers       = Js.Opt.option iceServers >|= (fun l -> List.map Js.string l |> Array.of_list) in 
  let iceServers'      = ("iceServers",      iceServers |> inject) in
  let ipv6'            = ("ipv6",            Js.Opt.option ipv6 >|= Js.bool |> inject) in
  let withCredentials' = ("withCredentials", Js.Opt.option withCredentials >|= Js.bool |> inject) in
  let max_poll_events' = ("max_poll_events", Js.Opt.option max_poll_events >|= Js.number_of_float |> inject) in
  let destroyOnUnload' = ("destroyOnUnload", Js.Opt.option destroyOnUnload >|= Js.bool |> inject) in
  let token'           = ("token",           Js.Opt.option token >|= Js.string |> inject) in
  let apisecret'       = ("apisecret",       Js.Opt.option apisecret >|= Js.string |> inject) in
  let success'         = ("success",         Js.Opt.option success >|= Js.wrap_callback |> inject) in
  let error'           = ("error",           Js.Opt.option error >|= Js.wrap_callback |> inject) in
  let destroyed'       = ("destroyed",       Js.Opt.option destroyed >|= Js.wrap_callback |> inject) in
  new_obj Js.Unsafe.global##._Janus [| obj [| iceServers'; ipv6'; withCredentials'; max_poll_events';
                                              destroyOnUnload'; token'; apisecret'; success';
                                              error'; destroyed'; server' |] |]
