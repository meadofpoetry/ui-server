let (>|=) x f = Js.Optdef.map x f

module Mp = struct

  (* RTP mountpoint parameters *)
  type rtp_audio =
    { audiomcast : string option
    ; audioport : int
    ; audiopt : int
    ; audiortpmap : string
    ; audiofmtp : string option
    ; audioiface : string option
    }

  type rtp_video =
    { videomcast : string option
    ; videoport : int
    ; videopt : int
    ; videortpmap : string
    ; videofmpt : string option
    ; videoiface : string option
    ; videobufferkf : bool option
    }

  type rtp_data =
    { dataport : int
    ; databuffermsg : bool option
    ; dataiface : string option
    }

  type rtp =
    { id          : int
    ; name        : string option
    ; description : string option
    ; is_private  : bool option
    ; audio       : bool option
    ; video       : bool option
    ; data        : bool option
    ; audio_info  : rtp_audio option
    ; video_info  : rtp_video option
    ; data_info   : rtp_data option
    }

  type mp_type = Rtp of rtp
               | Live
               | Ondemand
               | Rtsp

  (* Base mountpoint parameters *)
  type t =
    { info      : mp_type option
    ; secret    : string option
    ; pin       : string option
    ; admin_key : string option
    ; permanent : bool option
    }

end

type msg_data =
  [ `List
  | `Info of int
  | `Create of Mp.t
  | `Destroy of int * bool
  | `Recording of Mp.t * string * bool * bool * bool
  | `Enable of int
  | `Disable of int
  | `Watch of int
  | `Start of int
  | `Pause of int
  | `Stop of int
  | `Switch of int]

let request_type_to_string = function
  | `List        -> "list"      | `Info _   -> "info"   | `Create _  -> "create"  | `Destroy _ -> "destroy"
  | `Recording _ -> "recording" | `Enable _ -> "enable" | `Disable _ -> "disable" | `Watch _   -> "watch"
  | `Start _     -> "start"     | `Pause _  -> "pause"  | `Stop _    -> "stop"    | `Switch _  -> "switch"

(* let request_type_to_params request = *)
(*   let arr = match request with *)
(*     | `List -> [| |] *)
(*     | `Info id -> [| ("id", id) |] in *)
(*     (\* | `Create _ -> [| |](\\* TODO *\\) *\) *)
(*     (\* | `Destroy (id,permanent) ->  [| ("id", id), *\) *)
(*     (\*                                  ("permanent", Js.bool permanent) |] *\) *)
(*     (\* | `Recording (id,action,audio,video,data) -> [| ("id", id), *\) *)
(*     (\*                                                 ("action",action), *\) *)
(*     (\*                                                 ("audio", Js.bool audio), *\) *)
(*     (\*                                                 ("video", Js.bool video), *\) *)
(*     (\*                                                 ("data", Js.bool data) |] *\) *)
(*     (\* | (`Enable _ | `Disable _) as id -> [| ("id", id) |] *\) *)
(*     (\* | (`Watch _ | `Start _ | `Pause _ | `Stop _ | `Switch _ ) as id -> [| ("id", id) |] in *\) *)
(*   Js.Unsafe.obj arr *)

type plugin_msg
type answer_msg

let make_answer_msg
    ~jsep
    ?success
    ?error
    () =
  let open Js.Unsafe in
  let jsep' = ("jsep", inject jsep) in
  let media = ("media", obj [| ("audioSend", inject Js._false);
                               ("videoSend", inject Js._false) |]) in
  let success' = ("success", Js.Optdef.option success >|= Js.wrap_callback |> inject) in
  let error' = ("error", Js.Optdef.option error >|= Js.wrap_callback |> inject) in
  obj [| jsep'; media; success'; error' |]

let make_plugin_msg
    ~request
    ?jsep
    ?success
    ?error
    () =
  let open Js.Unsafe in
  let request' = [| ("request", Js.string (request_type_to_string request) |> inject) |] in
  let jsep' = ("jsep", Js.Optdef.option jsep |> inject) in
  let params = match request with
    | `List -> [||]
    | `Watch id -> [| ("id", inject id) |]
    | _ -> [||] in
  let message = ("message",  obj (Array.append request' params)) in
  let success' = ("success", Js.Optdef.option success >|= Js.wrap_callback |> inject) in
  let error' = ("error", Js.Optdef.option error >|= Js.wrap_callback |> inject) in
  obj [| message; jsep'; success'; error' |]

class type handle =
  object
    method getId        : unit        -> Js.number Js.t Js.meth
    method getPlugin    : unit        -> Js.js_string Js.t Js.meth
    method send         : plugin_msg  -> unit Js.meth
    method createAnswer : answer_msg  -> unit Js.meth
  end

type parameters

let make_parameters ~plugin
    ?opaqueId
    ?success
    ?error
    ?consentDialog
    ?webrtcState
    ?iceState
    ?mediaState
    ?slowLink
    ?onmessage
    ?onlocalstream
    ?onremotestream
    ?ondataopen
    ?ondata
    ?oncleanup
    ?detached
    () =
  let open Js.Unsafe in
  let plugin' = ("plugin", Js.string plugin |> inject) in
  let opaqueId' = ("opaqueId", Js.Optdef.option opaqueId >|= Js.string |> inject) in
  let success' = ("success", Js.Optdef.option success >|= Js.wrap_callback |> inject) in
  let error' = ("error", Js.Optdef.option error >|= Js.wrap_callback |> inject) in
  let consentDialog' = ("consentDialog", Js.Optdef.option consentDialog >|= Js.wrap_callback |> inject) in
  let webrtcState' = ("webrtcState", Js.Optdef.option webrtcState >|= Js.wrap_callback |> inject) in
  let iceState' = ("iceState", Js.Optdef.option iceState >|= Js.wrap_callback |> inject) in
  let mediaState' = ("mediaState", Js.Optdef.option mediaState >|= Js.wrap_callback |> inject) in
  let slowLink' = ("slowLink", Js.Optdef.option slowLink >|= Js.wrap_callback |> inject) in
  let onmessage' = ("onmessage", Js.Optdef.option onmessage >|= Js.wrap_callback |> inject) in
  let onlocalstream' = ("onlocalstream", Js.Optdef.option onlocalstream >|= Js.wrap_callback |> inject) in
  let onremotestream' = ("onremotestream", Js.Optdef.option onremotestream >|= Js.wrap_callback |> inject) in
  let ondataopen' = ("ondataopen", Js.Optdef.option ondataopen >|= Js.wrap_callback |> inject) in
  let ondata' = ("ondata", Js.Optdef.option ondata >|= Js.wrap_callback |> inject) in
  let oncleanup' = ("oncleanup", Js.Optdef.option oncleanup >|= Js.wrap_callback |> inject) in
  let detached' = ("detached", Js.Optdef.option detached >|= Js.wrap_callback |> inject) in
  obj [| plugin'; opaqueId'; success'; error'; consentDialog'; webrtcState';
         iceState'; mediaState'; slowLink'; onmessage'; onlocalstream';
         onremotestream'; ondataopen'; ondata'; oncleanup'; detached' |]

class type janus = 
  object
    method getServer     : unit       -> Js.js_string Js.t Js.meth
    method isConnected   : unit       -> bool Js.t Js.meth
    method getSessionId  : unit       -> Js.number Js.t Js.meth
    method attach        : parameters -> unit Js.meth
    method destroy       : parameters -> unit Js.meth
  end

  
let init ?debug ?callback () =
  let debug' = ("debug", Js.Optdef.option debug >|= Js.string |> Js.Unsafe.inject) in
  let cb    = (Js.Optdef.option callback) >|= Js.wrap_callback in
  Js.Unsafe.(
    fun_call (Js.Unsafe.js_expr "Janus.init") [|obj [| debug';
                                                       ("callback", inject cb)|] |]
  )

let isWebrtcSupported () = Js.Unsafe.fun_call (Js.Unsafe.js_expr "Janus.isWebrtcSupported") [||]

let attachMediaStream div stream =
  Js.Unsafe.(fun_call (js_expr "Janus.attachMediaStream") [| inject div; inject stream|])
  
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
  let iceServers       = Js.Optdef.option iceServers >|= (fun l -> List.map Js.string l |> Array.of_list) in 
  let iceServers'      = ("iceServers",      iceServers |> inject) in
  let ipv6'            = ("ipv6",            Js.Optdef.option ipv6 >|= Js.bool |> inject) in
  let withCredentials' = ("withCredentials", Js.Optdef.option withCredentials >|= Js.bool |> inject) in
  let max_poll_events' = ("max_poll_events", Js.Optdef.option max_poll_events >|= Js.number_of_float |> inject) in
  let destroyOnUnload' = ("destroyOnUnload", Js.Optdef.option destroyOnUnload >|= Js.bool |> inject) in
  let token'           = ("token",           Js.Optdef.option token >|= Js.string |> inject) in
  let apisecret'       = ("apisecret",       Js.Optdef.option apisecret >|= Js.string |> inject) in
  let success'         = ("success",         Js.Optdef.option success >|= Js.wrap_callback |> inject) in
  let error'           = ("error",           Js.Optdef.option error >|= Js.wrap_callback |> inject) in
  let destroyed'       = ("destroyed",       Js.Optdef.option destroyed >|= Js.wrap_callback |> inject) in
  new_obj Js.Unsafe.global##._Janus [| obj [| iceServers'; ipv6'; withCredentials'; max_poll_events';
                                              destroyOnUnload'; token'; apisecret'; success';
                                              error'; destroyed'; server' |] |]
