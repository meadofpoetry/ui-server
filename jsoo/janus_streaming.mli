(*  Base mountpoint parameters for any kind (rtp, live, ondemand) *)
module Mp_base : sig

  type t =
    { id          : int option
    ; name        : string option
    ; description : string option
    ; is_private  : bool option
    ; audio       : bool option
    ; video       : bool option
    ; data        : bool option
    }

  val to_js_obj : t -> (string * Js.Unsafe.any) array

end

(* Parameters for rtp mountpoint*)
module Mp_rtp : sig

  type audio =
    { audiomcast  : string option
    ; audioport   : int
    ; audiopt     : int
    ; audiortpmap : string
    ; audiofmtp   : string option
    ; audioiface  : string option
    }

  type video =
    { videomcast    : string option
    ; videoport     : int
    ; videopt       : int
    ; videortpmap   : string
    ; videofmtp     : string option
    ; videoiface    : string option
    ; videobufferkf : bool option
    }

  type data =
    { dataport      : int
    ; databuffermsg : bool option
    ; dataiface     : string option
    }
    
  type t = { base : Mp_base.t option
           ; audio : audio option
           ; video : video option
           ; data  : data option
           }

  val to_js_obj : t -> (string * Js.Unsafe.any) array

end

(* Parameters for live mountpoint *)
module Mp_live : sig

  type t =
    { base : Mp_base.t option
    ; filename : string
    }

  val to_js_obj : t -> (string * Js.Unsafe.any) array

end

(* Parameters for ondemand mountpoint *)
module Mp_ondemand : sig

  type t = Mp_live.t

  val to_js_obj : t -> (string * Js.Unsafe.any) array

end

(* Parameters for rtps mountpoint *)
module Mp_rtsp : sig

  type t =
    { base      : Mp_base.t option
    ; url       : string option
    ; rtsp_user : string option
    ; rtsp_pwd  : string option
    ; rtspiface : string option
    }

  val to_js_obj : t -> (string * Js.Unsafe.any) array

end

(** Request to return a list of all available mountpoints **)
module Mp_list : sig

  type t = unit

  type r = { id           : int option
           ; type_        : string option
           ; description  : string option
           ; video_age_ms : int option
           ; audio_age_ms : int option
           }

  val to_js_obj : t -> (string * Js.Unsafe.any) array
  val of_js_obj : 'a Js.t array -> (r list,string) Result.result

end

(** Request to return info on a specific mountpoint **)
module Mp_info : sig

  type t = int

  type r = { id           : int option
           ; type_        : string option
           ; description  : string option
           ; video_age_ms : int option
           ; audio_age_ms : int option
           ; data_age_ms  : int option
           ; video        : string option
           ; audio        : string option
           ; data         : string option
           }

  val to_js_obj : t -> (string * Js.Unsafe.any) array
  val of_js_obj : 'a Js.t -> (r,string) Result.result

end

(** Request to create a new stream **)
module Mp_create : sig

  type mp_type = Rtp of Mp_rtp.t
               | Live of Mp_live.t
               | Ondemand of Mp_ondemand.t
               | Rtsp of Mp_rtsp.t

  type t =
    { type_     : mp_type
    ; admin_key : string option
    ; secret    : string option
    ; pin       : string option
    ; permanent : bool option
    }

  type r = { id          : int option
           ; type_       : string option
           ; description : string option
           ; is_private  : bool option
           ; audio_port  : int option
           ; video_port  : int option
           ; data_port   : int option
           }

  val to_js_obj : t -> (string * Js.Unsafe.any) array
  val of_js_obj : 'a Js.t -> (r,string) Result.result

end

(** Request to get rid of an existing stream **)
module Mp_destroy : sig

  type t =
    { id        : int
    ; secret    : string option
    ; permanent : bool option
    }

  type r = int

  val to_js_obj : t -> (string * Js.Unsafe.any) array
  val of_js_obj : Js.number Js.t -> (r,string) Result.result

end

(** Request to start/stop recording of a live, RTP-based stream **)
module Mp_recording : sig

  type recording_action =
    | Start of (string option * string option * string option) (* filenames *)
    | Stop of (bool option * bool option * bool option) (* flags *)

  type t =
    { id     : int
    ; action : recording_action
    ; secret : string option
    }

  type r = unit

  val to_js_obj : t -> (string * Js.Unsafe.any) array
  val of_js_obj : Js.js_string Js.t -> (r,string) Result.result

end

(** A request to enable a mountpoint *)
module Mp_enable : sig

  type t = { id     : int
           ; secret : string option
           }

  type r = unit

  val to_js_obj : t -> (string * Js.Unsafe.any) array
  val of_js_obj : Js.js_string Js.t -> (r,string) Result.result

end

(** A request to disable a mountpoint *)
module Mp_disable : sig

  include (module type of Mp_enable)

end

(** A request to watch a stream **)
module Mp_watch : sig

  type t = { id     : int
           ; secret : string option
           }

  val to_js_obj : t -> (string * Js.Unsafe.any) array

end

(** A request to start a stream **)
module Mp_start : sig

  type t = unit

  val to_js_obj : t -> (string * Js.Unsafe.any) array

end

(** A request to pause a stream **)
module Mp_pause : sig

  include (module type of Mp_start)

end

(** A request to stop a stream **)
module Mp_stop : sig

  include (module type of Mp_start)

end

(** A request to switch to a different mountpoint **)
module Mp_switch : sig

  type t = int

  val to_js_obj : t -> (string * Js.Unsafe.any) array

end

(** Streaming plugin request types **)
type _ request =
  (* Sync *)
  | List      : Mp_list.r list request
  | Info      : Mp_info.t      -> Mp_info.r request
  | Create    : Mp_create.t    -> Mp_create.r request
  | Destroy   : Mp_destroy.t   -> Mp_destroy.r request
  | Recording : Mp_recording.t -> Mp_recording.r request
  | Enable    : Mp_enable.t    -> Mp_enable.r request
  | Disable   : Mp_disable.t   -> Mp_disable.r request
  (* Async *)
  | Watch     : Mp_watch.t     -> unit request
  | Start     : unit request
  | Pause     : unit request
  | Stop      : unit request
  | Switch    : Mp_switch.t    -> unit request

val send : ?jsep:'a -> Janus_static.Plugin.t -> 'b request -> ('b,string) Result.result Lwt.t
