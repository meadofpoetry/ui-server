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
  } [@@deriving eq]

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
  } [@@deriving eq]

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

type streams = Common.Stream.id list [@@deriving yojson,eq]

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
