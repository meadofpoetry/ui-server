open Common
open Board_types
open Containers

(** Mode **)

type mode =
  { input : input
  ; t2mi  : t2mi_mode option
  } [@@deriving eq]

(** Status **)

type status_versions =
  { streams_ver  : int
  ; ts_ver_com   : int
  ; ts_ver_lst   : int list
  ; t2mi_ver_lst : int list
  } [@@deriving show, eq]

type status_raw =
  { status       : status
  ; reset        : bool
  ; input        : input
  ; t2mi_mode    : t2mi_mode option
  ; jitter_mode  : jitter_mode option
  ; errors       : bool
  ; t2mi_sync    : int list
  ; version      : int
  ; versions     : status_versions
  ; streams      : Stream.id list
  } [@@deriving show, eq]

(** T2-MI errors **)

type t2mi_error_raw =
  { code      : int
  ; stream_id : int
  ; count     : int
  }

type t2mi_error_adv_raw =
  { code      : int
  ; stream_id : int
  ; param     : int
  }

(** Jitter **)

type jitter_raw =
  { measures  : Jitter.measures
  ; next_ptr  : int32
  ; time      : int
  ; timestamp : Time.t
  ; pid       : int
  ; t_pcr     : float
  }

(** Streams **)

type streams = Common.Stream.id list [@@deriving yojson,show,eq]

(** Event group **)

type event = [ `Status        of status_raw
             | `Streams_event of streams
             | `T2mi_errors   of Stream.id * (Errors.t list)
             | `Ts_errors     of Stream.id * (Errors.t list)
             | `End_of_errors
             ] [@@deriving eq]

type group =
  { status      : status_raw
  ; prev_status : status_raw option
  ; events      : event list
  }

(* API *)

type jitter_req =
  { request_id : int
  ; pointer    : int32
  }

type t2mi_info_req =
  { request_id : int
  ; stream_id  : int
  ; stream     : Stream.id
  }

type t2mi_frame_seq_req =
  { request_id : int
  ; params     : frame_seq_params
  }
and frame_seq_params =
  { seconds : int
  ; stream  : Stream.id
  }

type section_req =
  { request_id : int
  ; params     : section_params
  }
and section_params =
  { stream_id      : Common.Stream.id
  ; table_id       : int
  ; section        : int option (* needed for tables containing multiple sections *)
  ; table_id_ext   : int option (* needed for tables with extra parameter, like ts id for PAT *)
  ; eit_ts_id      : int option (* ts id for EIT *)
  ; eit_orig_nw_id : int option (* original network ID for EIT *)
  } [@@deriving yojson, show]

type api =
  { get_devinfo     : unit -> devinfo option
  ; set_input       : input -> input Lwt.t
  ; set_t2mi_mode   : t2mi_mode option -> t2mi_mode option Lwt.t
  ; set_jitter_mode : jitter_mode option -> jitter_mode option Lwt.t
  ; get_t2mi_seq    : frame_seq_params -> Streams.T2MI.sequence Lwt.t
  ; get_section     : section_params -> (Streams.TS.section,Streams.TS.section_error) Lwt_result.t
  ; reset           : unit -> unit Lwt.t
  ; config          : unit -> config
  }

(* Events *)

type device_events =
  { config : config React.event
  ; input  : input React.signal
  ; state  : Common.Topology.state React.signal
  ; status : status React.event
  ; reset  : reset_ts React.event
  ; errors : board_errors React.event
  }

type ts_events =
  { structures : (Stream.id * Streams.TS.structure) list React.event
  ; bitrates   : (Stream.id * Streams.TS.bitrate) list React.event
  ; errors     : (Stream.id * Errors.t list) list React.event
  }

type t2mi_events =
  { structures : (Stream.id * Streams.T2MI.structure) list React.event
  ; errors     : (Stream.id * Errors.t list) list React.event
  }

type jitter_events =
  { session : Jitter.session React.event
  ; jitter  : Jitter.measures React.event
  }

type events =
  { device  : device_events
  ; streams : Stream.t list React.signal
  ; ts      : ts_events
  ; t2mi    : t2mi_events
  ; jitter  : jitter_events
  }

type push_events =
  { devinfo        : devinfo option -> unit
  ; state          : Topology.state -> unit
  ; group          : group -> unit
  ; board_errors   : board_errors -> unit
  ; structs        : (Stream.id * Streams.TS.structure) list -> unit
  ; bitrates       : (Stream.id * Streams.TS.bitrate) list -> unit
  ; t2mi_info      : (Stream.id * Streams.T2MI.structure) list -> unit
  ; jitter         : Jitter.measures -> unit
  ; jitter_session : Jitter.session -> unit
  }
