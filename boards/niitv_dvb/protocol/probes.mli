open Board_niitv_dvb_types

(* TODO remove from here. *)
type push_events =
  { measure : (int * Measure.t ts) list -> unit
  ; params : (int * Params.t ts) list -> unit
  ; plp_list : (int * Plp_list.t ts) list -> unit
  ; state : Application_types.Topology.state -> unit
  ; devinfo : Device.info option -> unit
  }

type t

type event_raw =
  [ `Measure of int * Cstruct.t
  | `Params of int * Cstruct.t
  | `Plps of int * Cstruct.t
  ]

val handle_events : push_events -> t -> t

val send : t -> t

val apply : t -> event_raw list -> t

val update : (int * Device.standard) list -> t -> t

val is_empty : t -> bool

val is_last : t -> bool

val make : float ->
           (Parser.event Parser.event_request -> unit Lwt.t) ->
           (int * Device.standard) list ->
           int list ->
           t

val _match :
  t ->
  resolved:(t -> Parser.event -> 'a Lwt.t) ->
  timeout:(t -> 'a Lwt.t) ->
  pending:(t -> 'a Lwt.t) ->
  not_sent:(t -> 'a Lwt.t) ->
  'a Lwt.t
