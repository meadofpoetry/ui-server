open Common.Topology

(** Mode **)

type mode =
  { input : Board_types.input
  ; t2mi  : Board_types.t2mi_mode option
  } [@@deriving eq]

(** Status **)

type status_versions =
  { streams_ver  : int
  ; ts_ver_com   : int
  ; ts_ver_lst   : int list
  ; t2mi_ver_lst : int list
  } [@@deriving eq]

type status_raw =
  { status       : Board_types.Board.status
  ; reset        : bool
  ; input        : Board_types.input
  ; t2mi_mode    : Board_types.t2mi_mode option
  ; jitter_mode  : Board_types.jitter_mode option
  ; errors       : bool
  ; t2mi_sync    : int list
  ; versions     : status_versions
  ; streams      : Common.Stream.id list
  } [@@deriving eq]

(** TS errors **)

type ts_error_raw =
  { count     : int
  ; err_code  : int
  ; err_ext   : int
  ; multi_pid : bool
  ; pid       : int
  ; packet    : int32
  ; param_1   : int32
  ; param_2   : int32
  }

type ts_errors_raw =
  { timestamp : Common.Time.t
  ; stream_id : Common.Stream.id
  ; errors    : ts_error_raw list
  }

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

(** Bitrate **)

type pid_bitrate =
  { pid     : int
  ; bitrate : int
  }

type table_bitrate =
  { id             : int
  ; id_ext         : int
  ; fully_analyzed : bool
  ; section_syntax : bool
  ; eit_info       : (int * int) option
  ; bitrate        : int
  }

type bitrate =
  { stream_id  : Common.Stream.id
  ; ts_bitrate : int
  ; pids       : pid_bitrate list
  ; tables     : table_bitrate list
  ; timestamp  : Common.Time.t
  }

type bitrates = bitrate list

(** Jitter **)

type jitter_raw =
  { measures  : Board_types.jitter_measures
  ; next_ptr  : int32
  ; time      : int
  ; timestamp : Common.Time.t
  ; pid       : int
  ; t_pcr     : float
  }

(** Streams **)

type streams = Common.Stream.id list [@@deriving yojson]

(** Event group **)

type event = [ `Status        of status_raw
             | `Streams_event of streams
             | `T2mi_errors   of Board_types.t2mi_error list
             | `Ts_errors     of ts_errors_raw
             | `End_of_errors
             ]

type group =
  { status      : status_raw
  ; prev_status : status_raw option
  ; events      : event list
  }
