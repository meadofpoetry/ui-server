open Common.Topology

type connection_state = [ `Active | `Muted | `Sync ]

type point =
  { x : int
  ; y : int
  }

type node_entry = [ `CPU of topo_cpu | `Entry of topo_entry ]
