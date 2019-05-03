open Pipeline_types

module Event : sig

  val get_streams :
    ?f:(Api_js.Websocket.t
        -> (Structure.t list, string) result
        -> unit)
    -> ?applied:bool
    -> ?inputs:Application_types.Topology.topo_input list
    -> ?ids:Application_types.Stream.ID.t list
    -> unit
    -> (Api_js.Websocket.t, string) result Lwt.t

  val get_streams_with_source :
    ?f:(Api_js.Websocket.t
        -> (Structure.packed list, string) result
        -> unit)
    -> ?applied:bool
    -> ?inputs:Application_types.Topology.topo_input list
    -> ?ids:Application_types.Stream.ID.t list
    -> unit
    -> (Api_js.Websocket.t, string) result Lwt.t

end

val apply_streams : Structure.t list -> (unit, Api_js.Http.error) result Lwt.t

val get_streams : ?applied:bool
  -> ?inputs:Application_types.Topology.topo_input list
  -> ?ids:Application_types.Stream.ID.t list
  -> unit
  -> (Structure.t list, Api_js.Http.error) result Lwt.t

val get_streams_with_source : ?applied:bool
  -> ?inputs:Application_types.Topology.topo_input list
  -> ?ids:Application_types.Stream.ID.t list
  -> unit
  -> (Structure.packed list, Api_js.Http.error) result Lwt.t
