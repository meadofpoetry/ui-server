open Common
open Containers
open React

include Board_types

(** Init *)

type init =
  { input : int
  ; t2mi : int
  } [@@deriving of_yojson]

(** Mode *)

type t2mi_mode_raw =
  { enabled : bool
  ; pid : int
  ; t2mi_stream_id : int
  ; stream : Stream.Multi_TS_ID.t
  } [@@deriving show, eq]

let t2mi_mode_raw_default : t2mi_mode_raw =
  { pid = 0
  ; enabled = false
  ; t2mi_stream_id = 0
  ; stream = Stream.Multi_TS_ID.of_int32_pure 0l }

type mode =
  { input : input
  ; t2mi : t2mi_mode_raw option
  } [@@deriving eq]

(** Status *)

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

(** T2-MI errors *)

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

(** Jitter *)

type jitter_raw =
  { measures : Jitter.measures
  ; next_ptr : int32
  ; time : int
  ; timestamp : Time.t
  ; pid : int
  ; t_pcr : float
  }

(** Streams *)

type streams =
  Stream.Multi_TS_ID.t list [@@deriving yojson, show, eq]

(** TS *)

type structure =
  { info : Ts_info.t
  ; services : Service.t list
  ; tables : SI_PSI_table.t list
  ; pids : Pid.t list
  } [@@deriving yojson]

(** Event group *)

type board_event =
  [ `Status of status_raw
  | `Streams_event of streams
  | `T2mi_errors of Stream.Multi_TS_ID.t * (Error.t list)
  | `Ts_errors of Stream.Multi_TS_ID.t * (Error.t list)
  | `End_of_errors
  | `End_of_transmission
  ] [@@deriving eq, show]

type group =
  { status : status_raw
  ; prev_status : status_raw option
  ; events : board_event list
  } [@@deriving show]

(* Helper types *)

type bitrates = (Stream.ID.t * Bitrate.t timestamped) list
type ts_info = (Stream.ID.t * Ts_info.t timestamped) list
type services = (Stream.ID.t * (Service.t list timestamped)) list
type tables = (Stream.ID.t * (SI_PSI_table.t list timestamped)) list
type t2mi_info = (Stream.ID.t * T2mi_info.t list timestamped) list
type errors = (Stream.ID.t * (Error.t list)) list

type pids = (Stream.ID.t * Pid.t list timestamped) list [@@deriving eq, yojson]

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
  ; table_id_ext : int option (* needed for tables with extra parameter,
                               * like ts id for PAT *)
  ; ext_info_1 : int option (* ts_id for EIT, orig_nw_id for SDT *)
  ; ext_info_2 : int option (* orig_nw_id for EIT *)
  } [@@deriving yojson, show]

type api =
  { get_devinfo : unit -> devinfo option
  ; set_input : input -> input Lwt.t
  ; set_t2mi_mode : t2mi_mode option -> t2mi_mode option Lwt.t
  ; set_jitter_mode : jitter_mode option -> jitter_mode option Lwt.t
  ; get_t2mi_seq : frame_seq_params -> T2mi_sequence.t timestamped Lwt.t
  ; get_section :
      ?section:int ->
      ?table_id_ext:int ->
      ?ext_info_1:int ->
      ?ext_info_2:int ->
      id:Stream.ID.t ->
      table_id:int ->
      unit ->
      (SI_PSI_section.t timestamped,
       SI_PSI_section.dump_error) Lwt_result.t
  ; reset : unit -> unit Lwt.t
  ; config : unit -> config
  ; get_streams :
      ?ids:Stream.ID.t list ->
      ?incoming:bool ->
      ?inputs:Topology.topo_input list ->
      unit ->
      Stream.t list Lwt.t
  ; get_ts_info : unit -> ts_info Lwt.t
  ; get_pids : unit -> pids Lwt.t
  ; get_services : unit -> services Lwt.t
  ; get_tables : unit -> tables Lwt.t
  ; get_t2mi_info : unit -> t2mi_info Lwt.t
  }

(** Events *)

type device_events =
  { config : config signal
  ; t2mi_mode : t2mi_mode option signal
  ; t2mi_mode_raw : t2mi_mode_raw event
  ; jitter_mode : jitter_mode option signal
  ; input : input signal
  ; state : Topology.state signal
  ; status : status event
  ; reset : reset_ts event
  ; errors : board_error list event
  ; info : devinfo option signal
  }

type ts_events =
  { info : ts_info signal
  ; services : services signal
  ; tables : tables signal
  ; pids : pids signal
  ; bitrates : bitrates event
  ; errors : errors event
  }

type t2mi_events =
  { structures : t2mi_info signal
  ; errors : (Stream.ID.t * Error.t list) list event
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
  ; t2mi_mode : t2mi_mode option -> unit
  ; jitter_mode : jitter_mode option -> unit
  ; group : group -> unit
  ; board_errors : board_error list -> unit
  ; info : ts_info -> unit
  ; services : services -> unit
  ; tables : tables -> unit
  ; pids : pids -> unit
  ; bitrates : bitrates -> unit
  ; t2mi_info : t2mi_info -> unit
  ; jitter : Jitter.measures -> unit
  ; jitter_session : Jitter.session -> unit
  }
