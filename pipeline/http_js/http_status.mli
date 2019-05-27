open Pipeline_types

module Event : sig

  val get :
    ?f:(Api_js.Websocket.t
        -> (Qoe_status.t list, string) result
        -> unit)
    -> ?ids:Application_types.Stream.ID.t list
    -> unit
    -> (Api_js.Websocket.t, string) result Lwt.t

end

val get :
  ?ids:Application_types.Stream.ID.t list
  -> unit
  -> (Qoe_status.t list, Api_js.Http.error) result Lwt.t
