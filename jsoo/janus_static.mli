(* (\* Base mountpoint parameters for any kind (rtp, live, ondemand) *\) *)
(* module Mp_base : sig *)

(*   type t = *)
(*     { id          : int option *)
(*     ; name        : string option *)
(*     ; description : string option *)
(*     ; is_private  : bool option *)
(*     ; audio       : bool option *)
(*     ; video       : bool option *)
(*     ; data        : bool option *)
(*     } *)

(* end *)

(* (\* Parameters for rtp mountpoint*\) *)
(* module Mp_rtp : sig *)

(*   type t = { base : Mp_base.t option } *)

(*   type audio = *)
(*     { audiomcast  : string option *)
(*     ; audioport   : int *)
(*     ; audiopt     : int *)
(*     ; audiortpmap : string *)
(*     ; audiofmtp   : string option *)
(*     ; audioiface  : string option *)
(*     } *)

(*   type video = *)
(*     { videomcast    : string option *)
(*     ; videoport     : int *)
(*     ; videopt       : int *)
(*     ; videortpmap   : string *)
(*     ; videofmpt     : string option *)
(*     ; videoiface    : string option *)
(*     ; videobufferkf : bool option *)
(*     } *)

(*   type data = *)
(*     { dataport      : int *)
(*     ; databuffermsg : bool option *)
(*     ; dataiface     : string option *)
(*     } *)

(* end *)

(* (\* Parameters for live mountpoint *\) *)
(* module Mp_live : sig *)

(*   type t = *)
(*     { base : Mp_base.t option *)
(*     ; filename : string *)
(*     } *)

(* end *)

(* (\* Parameters for ondemand mountpoint *\) *)
(* module Mp_ondemand : sig *)

(*   type t = Mp_live.t *)

(* end *)

(* (\* Parameters for rtps mountpoint *\) *)
(* module Mp_rtsp : sig *)

(*   type t = *)
(*     { base      : Mp_base.t option *)
(*     ; url       : string option *)
(*     ; rtsp_user : string option *)
(*     ; rtsp_pwd  : string option *)
(*     ; rtspiface : string option *)
(*     } *)

(* end *)

(* (\* Parameters to start recording from a mountpoint *\) *)
(* module Mp_recording : sig *)

(*   type recording_action = Start *)
(*                         | Stop *)

(*   type t = *)
(*     { id     : int *)
(*     ; action : recording_action *)
(*     ; secret : string option *)
(*     ; audio  : bool option *)
(*     ; video  : bool option *)
(*     ; data   : bool option *)
(*     } *)

(* end *)

(* module Mp_create : sig *)

(*   type mp_type = Rtp *)
(*                | Live *)
(*                | Ondemand *)
(*                | Rtsp *)

(*   type t = *)
(*     { _type     : mp_type *)
(*     ; adminkey  : string option *)
(*     ; secret    : string option *)
(*     ; pin       : string option *)
(*     ; permanent : bool option *)
(*     } *)

(* end *)

(* (\* Parameters to destroy a mountpoint *\) *)
(* module Mp_destroy : sig *)

(*   type t = *)
(*     { id        : int *)
(*     ; secret    : string option *)
(*     ; permanent : bool option *)
(*     } *)

(* end *)


(* Janus plugin handler *)
module Plugin : sig

  (* Handler *)
  type t = Janus.plugin Js.t

  (* Mountpoint info *)
  type mp_info = { video_age_ms : int
                 ; audio_age_ms : int
                 ; id           : int
                 ; _type        : string
                 ; description  : string
                 }

  type media_video = Bool of bool
                   | Resolution of ([`Lowres | `Stdres | `Hires] * [`Wide | `Square ])
                   | Screen
                   | Device of (int * int * int)

  type media_audio = Bool of bool
                   | Device of int

  type media_props = { audio_send : bool option
                     ; audio_recv : bool option
                     ; audio : media_audio option
                     ; video_send : bool option
                     ; video_recv : bool option
                     ; video : media_video option
                     ; data : bool option
                     ; fail_if_no_video : bool option
                     ; fail_if_no_audio : bool option
                     ; screen_rate : int option
                     }

  (* Plugin request types *)
  type _ request =
    | List      : mp_info list request
    | Info      : int -> mp_info request
    | Create    : int -> string request
    | Destroy   : int -> string request
    | Recording : int -> string request
    | Enable    : int -> string request
    | Disable   : int -> string request
    | Watch     : int -> string request
    | Start     : unit request
    | Pause     : unit request
    | Stop      : unit request
    | Switch    : int -> unit request

  (* Get plugin id *)
  val get_id : t   -> int64

  (* Get plugin name *)
  val get_name : t -> string

  (* Send message to plugin *)
  val send  :  ?jsep:Js.json Js.t -> t -> 'a request -> 'a Lwt.t

  (* Create WebRTC answer *)
  val create_answer : t -> media_props -> bool option -> Js.json Js.t -> unit

end

(* Janus instance *)
type janus = Janus.janus Js.t

(* Available Janus debuggers *)
type debug_token = Trace
                 | Debug
                 | Log
                 | Warn
                 | Error

type create_result = { error   : string Lwt.t
                     ; success : janus Lwt.t
                     ; destroy : unit Lwt.t
                     }

type attach_result = { error : string Lwt.t
                     ; success : Plugin.t Lwt.t
                     }

type session_props = { server : [`One of string | `Many of string list]
                     ; ice_servers : string list option
                     ; ipv6 : bool option
                     ; with_credentials : bool option
                     ; max_poll_events : int option
                     ; destroy_on_unload : bool option
                     ; token : string option
                     ; apisecret : string option
                     }

type 'a react_push = ('a -> unit)

type plugin_props = { name : string
                    ; opaque_id : string option
                    ; consent_dialog : bool react_push option
                    ; webrtc_state : bool react_push option
                    ; ice_state : string react_push option
                    ; media_state : (string * bool) react_push option
                    ; slow_link : bool react_push option
                    ; on_cleanup : unit react_push option
                    ; detached : unit react_push option
                    }

(* Initialize Janus *)
val init : [`All of bool | `Several of debug_token list ] -> unit Lwt.t

(* Create Janus session *)
val create : session_props -> create_result

(* Attach plugin to Janus instance *)
val attach : janus -> plugin_props -> attach_result

