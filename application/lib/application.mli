type t =
  { proc : Data_processor.t option
  (* ; network : Pc_control.Network.t*)
  ; users : User.entries
  ; hw : Hardware.t
  ; db : Database.Conn.t
  ; topo : Common.Topology.t signal
  }

val create : Kv.RW.t -> Db.state -> (t, [> `Msg of string ]) Lwt_result.t
