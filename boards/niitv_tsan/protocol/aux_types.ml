open Application_types
open Board_niitv_tsan_types
open Request

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
  } [@@deriving eq]

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
  } [@@deriving eq]

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

type group =
  { status : status_raw
  ; prev_status : status_raw option
  ; events : board_event list
  }
and board_event =
  [ `Status of status_raw
  | `Streams_event of Stream.Multi_TS_ID.t list
  | `T2mi_errors of Stream.Multi_TS_ID.t * (Error.t list)
  | `Ts_errors of Stream.Multi_TS_ID.t * (Error.t list)
  | `End_of_errors
  | `End_of_transmission
  ] [@@deriving eq]


(** Events *)

type device_events =
  { config : config React.signal
  ; t2mi_mode : t2mi_mode option React.signal
  ; jitter_mode : jitter_mode option React.signal
  ; state : Topology.state React.signal
  ; input : input React.signal
  ; status : status React.signal
  ; errors : Board_error.t list React.event
  ; info : devinfo option React.signal
  }

type ts_events =
  { info : ts_info React.signal
  ; services : services React.signal
  ; tables : tables React.signal
  ; sections : sections React.signal
  ; pids : pids React.signal
  ; bitrates : bitrates React.event
  ; errors : (Stream.ID.t * (Error.t_ext list)) list React.event
  }

type t2mi_events =
  { structures : t2mi_info React.signal
  ; errors : errors React.event
  }

type jitter_events =
  { session : Jitter.session React.event
  ; jitter : Jitter.measures React.event
  }

type raw_events =
  { structures : (Stream.Multi_TS_ID.t * structure) list React.signal
  ; t2mi_mode_raw : t2mi_mode_raw React.event
  }

type notifs =
  { device : device_events
  ; streams : Stream.t list React.signal
  ; ts : ts_events
  ; t2mi : t2mi_events
  ; raw : raw_events
  ; jitter : jitter_events
  }
