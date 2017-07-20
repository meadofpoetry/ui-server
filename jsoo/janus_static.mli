(* Janus plugin handler *)
module Plugin : sig

  (* Types *)

  (** Plugin handler **)
  type t = Janus.plugin Js.t

  (** Possible plugin types **)
  type plugin_type = Audiobridge
                   | Echotest
                   | Recordplay
                   | Sip
                   | Streaming
                   | Textroom
                   | Videocall
                   | Videoroom
                   | Voicemail

  (** Possible video representations for WebRTC ANSWER/OFFER **)
  type media_video = Bool of bool
                   | Resolution of ([`Lowres | `Stdres | `Hires] * [`Wide | `Square ])
                   | Screen
                   | Device of (int * int * int)

  (** Possible audio representation for WebRTC ANSWER/OFFER **)
  type media_audio = Bool of bool
                   | Device of int

  (** Media properties for WebRTC ANSWER/OFFER **)
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

  (** Possible plugin response to a message sent **)
  type 'a send_response = Error of int option * string
                        | Empty
                        | Data of 'a

  (* Helper functions *)

  val parse_sync_response :
    string                            -> (* plugin name *)
    ('a -> ('b,string) Result.result) -> (* parser function called if data is present *)
    'd send_response                  -> (* typed response *)
    ('b,string) Result.result            (* returned result *)

  val parse_async_response : 'a send_response -> (unit,string) Result.result

  val data_or_error : 'a Js.t Js.optdef -> 'a Js.t send_response

  (* Plugin functions *)

  (** Get plugin id **)
  val get_id : t -> int64

  (** Get plugin name (with 'janus.plugin.' prefix) **)
  val get_name : t -> string

  (** Send a message to plugin **)
  val send :
    ?jsep:'a                                -> (* jsep *)
    t                                       -> (* plugin *)
    'b                                      -> (* request *)
    ('b -> string)                          -> (* request to string fn *)
    ('b -> (string * Js.Unsafe.any) array)  -> (* request to params fn *)
    ('c -> 'b -> ('d,string) Result.result) -> (* parse response fn *)
    (('d,string) Result.result) Lwt.t            (* result *)

  (** Ask Janus to create a WebRTC compliant ANSWER **)
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

type plugin_props = { name : Plugin.plugin_type
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

