open Common.Topology

(** Mode **)

type mode =
  { input : Board_types.input
  ; t2mi  : Board_types.t2mi_mode option
  }

(** Status **)

type status_versions =
  { streams_ver  : int
  ; ts_ver_com   : int
  ; ts_ver_lst   : int list
  ; t2mi_ver_lst : int list
  }

type status =
  { status    : Board_types.status
  ; errors    : bool
  ; t2mi_sync : int list
  ; versions  : status_versions
  ; streams   : Common.Stream.id list
  }

(** TS errors **)


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
  ; timestamp  : Common.Time.Seconds.t
  }

type bitrates = bitrate list


(** Streams **)

type streams = Common.Stream.id list [@@deriving yojson]
