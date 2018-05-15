open Common

type ('a,'b) rsp = ('a,'b Api_js.Requests.err) Lwt_result.t

type time_period = Time.Seconds.t * Time.Seconds.t

type 'a ws = 'a React.event * WebSockets.webSocket Js.t

module Wm_api : sig

  val get_wm    : unit -> (Wm.t,'a) rsp
  val get_wm_ws : unit -> Wm.t ws
  val set_wm    : Wm.t -> (unit,'a) rsp

end

module Settings_api : sig

  val set_settings : Settings.t -> unit -> (unit,'a) rsp

  module Real_time : sig

    val get_settings    : unit -> (Settings.t,'a) rsp
    val get_settings_ws : unit -> Settings.t ws

  end

  module Archive : sig

  end

end

module Structure_api : sig

  val set_structure : Structure.t list -> unit -> (unit,'a) rsp

  module Real_time : sig

    val get_structures    : unit     -> (Structure.t list,'a) rsp
    val get_structure     : Stream.t -> unit -> (Structure.t,'a) rsp

    val get_structures_ws : unit     -> Structure.t list ws
    val get_structure_ws  : Stream.t -> unit -> Structure.t ws

  end

  module Archive : sig

    type structures = Structure.t list * Time.t
    type structures_response =
      { structures : structures list
      ; has_more   : bool
      }

    type structure = Structure.t * Time.t
    type structure_response =
      { structure : structure list
      ; has_more  : bool
      }

    val get_structures          : time_period -> unit -> (structures_response,'a) rsp
    val get_structures_for_time : Time.t      -> unit -> (structures option,'a) rsp
    val get_structures_last     : unit        -> (structures option,'a) rsp

    val get_structure           : Stream.t -> time_period -> unit -> (structure_response,'a) rsp
    val get_structure_for_time  : Stream.t -> Time.t      -> unit -> (structure option,'a) rsp
    val get_structure_last      : Stream.t -> unit -> (structure option,'a) rsp

  end

end

module Errors_api : sig

  open Qoe_errors_types

  module Real_time : sig

    val get_video_data_for_stream_ws  : Stream.t -> unit -> Video_data.t ws
    val get_video_data_for_channel_ws : Stream.t -> int -> unit -> Video_data.t ws
    val get_video_data_for_pid_ws     : Stream.t -> int -> int -> unit -> Video_data.t ws

    val get_audio_data_for_stream_ws  : Stream.t -> unit -> Audio_data.t ws
    val get_audio_data_for_channel_ws : Stream.t -> int -> unit -> Audio_data.t ws
    val get_audio_data_for_pid_ws     : Stream.t -> int -> int -> unit -> Audio_data.t ws

  end

  module Archive : sig

  end

end
