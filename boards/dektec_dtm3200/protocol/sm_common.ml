open Board_dektec_dtm3200_types

type push_events =
  { state : Application_types.Topology.state -> unit
  ; status : status -> unit
  ; devinfo : devinfo option -> unit
  }
