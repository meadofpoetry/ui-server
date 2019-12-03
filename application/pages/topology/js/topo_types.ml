open Application_types.Topology

type connection_state =
  [ `Active (* path is active, but there is not info about streams *)
  | `Muted (* path is not active*)
  | `Sync (* path is active, streams detected *)
  | `Sync_lost (* path is active, no streams *)
  | `Unavailable (* path parent is not responding/unavailable *) ]
[@@deriving show]

type point =
  { x : int
  ; y : int }

type connection_point =
  [ `Iface of topo_interface
  | `Port of topo_port ]

type node_entry =
  [ `CPU of topo_cpu
  | `Entry of topo_entry ]

type topo_settings = Components.Widget.t * (unit -> unit)
