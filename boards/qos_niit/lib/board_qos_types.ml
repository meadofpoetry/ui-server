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
  { basic : status
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
  ; time : Time.t
  } [@@deriving yojson, eq]

(** Event group *)

type group =
  { status : status_raw
  ; prev_status : status_raw option
  ; events : board_event list
  }
and board_event =
  [ `Status of status_raw
  | `Streams_event of streams
  | `T2mi_errors of Stream.Multi_TS_ID.t * (Error.t list)
  | `Ts_errors of Stream.Multi_TS_ID.t * (Error.t list)
  | `End_of_errors
  | `End_of_transmission
  ] [@@deriving eq, show]

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
  ; table_id_ext : int option
  ; id_ext_1 : int option (* ts_id for EIT, orig_nw_id for SDT *)
  ; id_ext_2 : int option (* orig_nw_id for EIT *)
  ; section : int option
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
      ?id_ext_1:int ->
      ?id_ext_2:int ->
      id:Stream.ID.t ->
      table_id:int ->
      unit ->
      (SI_PSI_section.Dump.t timestamped,
       SI_PSI_section.Dump.error) Lwt_result.t
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
  ; get_sections : unit -> sections Lwt.t
  ; get_tables : unit -> tables Lwt.t
  ; get_t2mi_info : unit -> t2mi_info Lwt.t
  }

(** Events *)

type device_events =
  { config : config signal
  ; t2mi_mode : t2mi_mode option signal
  ; jitter_mode : jitter_mode option signal
  ; state : Topology.state signal
  ; input : input signal
  ; status : status signal
  ; errors : Board_error.t list event
  ; info : devinfo option signal
  }

type ts_events =
  { info : ts_info signal
  ; services : services signal
  ; tables : tables signal
  ; sections : sections signal
  ; pids : pids signal
  ; bitrates : bitrates event
  ; errors : (Stream.ID.t * (Error.t_ext list)) list event
  }

type t2mi_events =
  { structures : t2mi_info signal
  ; errors : errors event
  }

type jitter_events =
  { session : Jitter.session event
  ; jitter : Jitter.measures event
  }

type raw_events =
  { structures : (Stream.Multi_TS_ID.t * structure) list signal
  ; t2mi_mode_raw : t2mi_mode_raw event
  }

type events =
  { device : device_events
  ; streams : Stream.t list signal
  ; ts : ts_events
  ; t2mi : t2mi_events
  ; raw : raw_events
  ; jitter : jitter_events
  }

type push_events =
  { devinfo : devinfo option -> unit
  ; status : status -> unit
  ; input : input -> unit
  ; state : Topology.state -> unit
  ; t2mi_mode_raw : t2mi_mode_raw -> unit
  ; t2mi_mode : t2mi_mode option -> unit
  ; jitter_mode : jitter_mode option -> unit
  ; raw_streams : Stream.Raw.t list -> unit
  ; ts_errors : (Stream.ID.t * (Error.t_ext list)) list -> unit
  ; t2mi_errors : errors -> unit
  ; board_errors : Board_error.t list -> unit
  ; bitrates : bitrates -> unit
  ; t2mi_info : t2mi_info -> unit
  ; structures : (Stream.Multi_TS_ID.t * structure) list -> unit
  ; jitter : Jitter.measures -> unit
  ; jitter_session : Jitter.session -> unit
  }

(** Protocol messages *)

type _ instant_request =
  | Set_board_init : init -> unit instant_request
  | Set_board_mode : input * t2mi_mode_raw -> unit instant_request
  | Set_jitter_mode : jitter_mode option -> unit instant_request
  | Reset : unit instant_request

type probe_response =
  | Board_errors of Board_error.t list
  | Bitrate of (Stream.Multi_TS_ID.t * Bitrate.t) list
  | Struct of (Stream.Multi_TS_ID.t * structure) list
  | T2mi_info of (Stream.Multi_TS_ID.t * T2mi_info.t)
  | Jitter of jitter_raw

type _ probe_request =
  | Get_board_errors : int -> probe_response probe_request
  | Get_jitter : jitter_req -> probe_response probe_request
  | Get_ts_struct : ts_struct_req -> probe_response probe_request
  | Get_bitrates : int -> probe_response probe_request
  | Get_t2mi_info : t2mi_info_req -> probe_response probe_request

type _ request =
  | Get_board_info : devinfo request
  | Get_board_mode : (input * t2mi_mode_raw) request
  | Get_t2mi_frame_seq : t2mi_frame_seq_req -> T2mi_sequence.t timestamped request
  | Get_section : section_req ->
                  (SI_PSI_section.Dump.t timestamped,
                   SI_PSI_section.Dump.error) result request
