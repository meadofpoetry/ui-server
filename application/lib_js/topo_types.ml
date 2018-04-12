open Common.Topology

type connection_state = [ `Active | `Muted | `Sync | `Unavailable ]

type point =
  { x : int
  ; y : int
  }

type connection_point = [ `Iface of topo_interface | `Port of topo_port ]

type node_entry = [ `CPU of topo_cpu | `Entry of topo_entry ]

type topo_settings = Components.Widget.widget * (unit -> unit)
