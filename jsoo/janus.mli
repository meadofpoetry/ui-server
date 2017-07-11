(* Plugin handler object *)


module Mp : sig

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

(* types of plugin messages *)
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

val request_type_to_string : msg_data -> string

(* val request_type_to_params : msg_data -> Js.Unsafe.any *)

type plugin_msg
type answer_msg

val make_plugin_msg :
  request:msg_data ->
  ?jsep:Js.json Js.t ->
  ?success:(Js.json Js.t -> unit) ->
  ?error:(Js.json Js.t -> unit)   ->
  unit -> plugin_msg

val make_answer_msg :
  jsep:Js.json Js.t ->
  ?success:(Js.json Js.t -> unit) ->
  ?error:(Js.js_string Js.t -> unit) ->
  unit -> answer_msg

class type handle =
  object
    method getId        : unit         -> Js.number Js.t Js.meth
    method getPlugin    : unit         -> Js.js_string Js.t Js.meth
    method send         : plugin_msg   -> unit Js.meth
    method createAnswer : answer_msg   -> unit Js.meth
  end

(* Janus object*)

type parameters

val make_parameters : plugin:string ->
  ?opaqueId:string                                      ->
  ?success:(handle Js.t -> unit)                        ->
  ?error:(Js.js_string Js.t -> unit)                    ->
  ?consentDialog:(bool Js.t -> unit)                    ->
  ?webrtcState:(bool Js.t -> unit)                      ->
  ?iceState:(Js.js_string Js.t -> unit)                 ->
  ?mediaState:(Js.js_string Js.t -> bool Js.t -> unit)  ->
  ?slowLink:(bool Js.t-> unit)                          ->
  ?onmessage:(Js.json Js.t -> Js.json Js.t Js.opt Js.optdef -> unit) ->
  ?onlocalstream:(Js.json Js.t -> unit)                 ->
  ?onremotestream:(Js.json Js.t -> unit)                ->
  ?ondataopen:(Js.js_string Js.t -> unit)               ->
  ?ondata:(Js.js_string Js.t -> unit)                   ->
  ?oncleanup:(unit -> unit)                             ->
  ?detached:(unit -> unit)                              ->
  unit -> parameters

class type janus = 
  object
    method getServer     : unit       -> Js.js_string Js.t Js.meth
    method isConnected   : unit       -> bool Js.t Js.meth
    method getSessionId  : unit       -> Js.number Js.t Js.meth
    method attach        : parameters -> unit Js.meth
    method destroy       : parameters -> unit Js.meth
  end

val create : server:string            ->
             ?iceServers:string list  ->
             ?ipv6:bool               ->
             ?withCredentials:bool    ->
             ?max_poll_events:float   ->
             ?destroyOnUnload:bool    ->
             ?token:string            ->
             ?apisecret:string        ->
             ?success:(unit -> unit)       ->
             ?error:(Js.js_string Js.t -> unit) ->
             ?destroyed:(unit -> unit)     ->
             unit -> janus Js.t

(* Janus initialization *)
val init : ?debug:string -> ?callback:(unit -> unit) -> unit -> unit

(* Helper method to check whether WebRTC is supported by the browser *)
val isWebrtcSupported : unit -> bool

(* Helper method to attach a stream to a html video element *)
val attachMediaStream : Dom_html.element Js.t Js.opt -> Js.json Js.t -> unit
