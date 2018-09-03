open Common
open Board_types
open Containers

(** Init *)

type init =
  { input : int
  ; t2mi : int
  } [@@deriving of_yojson]

(** Mode **)

type t2mi_mode_raw =
  { enabled : bool
  ; pid : int
  ; t2mi_stream_id : int
  ; stream : Stream.Multi_TS_ID.t
  } [@@deriving show, eq]

type mode =
  { input : input
  ; t2mi : t2mi_mode_raw option
  } [@@deriving eq]

(** Status **)

type status_versions =
  { streams_ver : int
  ; ts_ver_com : int
  ; ts_ver_lst : int list
  ; t2mi_ver_lst : int list
  } [@@deriving show, eq]

type status_raw =
  { status : status
  ; reset : bool
  ; input : input
  ; t2mi_mode : t2mi_mode_raw
  ; jitter_mode : jitter_mode option
  ; errors : bool
  ; t2mi_sync : int list
  ; version : int
  ; versions : status_versions
  ; streams : Stream.Multi_TS_ID.t list
  } [@@deriving show, eq]

(** T2-MI errors **)

type t2mi_error_raw =
  { code : int
  ; stream_id : int
  ; count : int
  }

type t2mi_error_adv_raw =
  { code : int
  ; stream_id : int
  ; param : int
  }

(** Jitter **)

type jitter_raw =
  { measures : Jitter.measures
  ; next_ptr : int32
  ; time : int
  ; timestamp : Time.t
  ; pid : int
  ; t_pcr : float
  }

(** Streams **)

type streams = Stream.Multi_TS_ID.t list [@@deriving yojson,show,eq]

(** Event group **)

type event =
  [ `Status of status_raw
  | `Streams_event of streams
  | `T2mi_errors of Stream.Multi_TS_ID.t * (Errors.t list)
  | `Ts_errors of Stream.Multi_TS_ID.t * (Errors.t list)
  | `End_of_errors
  | `End_of_transmission
  ] [@@deriving eq, show]

type group =
  { status : status_raw
  ; prev_status : status_raw option
  ; events : event list
  } [@@deriving show]

(* API *)

type jitter_req =
  { request_id : int
  ; pointer : int32
  } [@@deriving show]

type ts_struct_req =
  { request_id : int
  ; stream : [ `All | `Single of Stream.Multi_TS_ID.t ]
  } [@@deriving show]

type t2mi_info_req =
  { request_id : int
  ; stream_id : int
  ; stream : Stream.Multi_TS_ID.t
  } [@@deriving show]

type t2mi_frame_seq_req =
  { request_id : int
  ; params : frame_seq_params
  }
and frame_seq_params =
  { seconds : int
  ; stream : Stream.ID.t
  } [@@deriving show]

type section_req =
  { request_id : int
  ; params : section_params
  }
and section_params =
  { stream_id : Stream.Multi_TS_ID.t
  ; table_id : int
  ; section : int option (* needed for tables containing multiple sections *)
  ; table_id_ext : int option (* needed for tables with extra parameter, like ts id for PAT *)
  ; ext_info_1 : int option (* ts_id for EIT, orig_nw_id for SDT *)
  ; ext_info_2 : int option (* orig_nw_id for EIT *)
  } [@@deriving yojson, show]

type api =
  { get_devinfo : unit -> devinfo option
  ; set_input : input -> input Lwt.t
  ; set_t2mi_mode : t2mi_mode option -> t2mi_mode option Lwt.t
  ; set_jitter_mode : jitter_mode option -> jitter_mode option Lwt.t
  ; get_t2mi_seq : frame_seq_params -> Streams.T2MI.sequence Lwt.t
  ; get_section :
      ?section:int ->
      ?table_id_ext:int ->
      ?ext_info_1:int ->
      ?ext_info_2:int ->
      id:Stream.ID.t ->
      table_id:int ->
      unit ->
      (Streams.TS.section, Streams.TS.section_error) Lwt_result.t
  ; reset : unit -> unit Lwt.t
  ; config : unit -> config
  }

(* Events *)

open React
open Streams.TS

type device_events =
  { config : config signal
  ; input : input signal
  ; state : Topology.state signal
  ; status : status event
  ; reset : reset_ts event
  ; errors : board_error list event
  ; info : devinfo option signal
  }

type ts_events =
  { info : (Stream.t * info) list event
  ; services : (Stream.t * services) list event
  ; tables : (Stream.t * tables) list event
  ; pids : (Stream.t * pids) list event
  ; bitrates : (Stream.t * bitrate) list event
  ; errors : (Stream.t * Errors.t list) list event
  }

type t2mi_events =
  { structures : (Stream.t * Streams.T2MI.structure) list event
  ; errors : (Stream.t * Errors.t list) list event
  }

type jitter_events =
  { session : Jitter.session event
  ; jitter : Jitter.measures event
  }

type events =
  { device : device_events
  ; streams : Stream.t list signal
  ; ts : ts_events
  ; t2mi : t2mi_events
  ; jitter : jitter_events
  }

type push_events =
  { devinfo : devinfo option -> unit
  ; state : Topology.state -> unit
  ; group : group -> unit
  ; board_errors : board_error list -> unit
  ; info : (Stream.Multi_TS_ID.t * info) list -> unit
  ; services : (Stream.Multi_TS_ID.t * services) list -> unit
  ; tables : (Stream.Multi_TS_ID.t * tables) list -> unit
  ; pids : (Stream.Multi_TS_ID.t * pids) list -> unit
  ; bitrates : (Stream.Multi_TS_ID.t * bitrate) list -> unit
  ; t2mi_info : (Stream.Multi_TS_ID.t * Streams.T2MI.structure) list -> unit
  ; jitter : Jitter.measures -> unit
  ; jitter_session : Jitter.session -> unit
  }
