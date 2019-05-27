open Pipeline_types

module Event : sig

  val get :
    ?f:(Api_js.Websocket.t
        -> (Wm.t, string) result
        -> unit)
    -> unit
    -> (Api_js.Websocket.t, string) result Lwt.t

end

val set_layout : Wm.t -> (unit, Api_js.Http.error) result Lwt.t

val get_layout : unit -> (Wm.t, Api_js.Http.error) result Lwt.t
