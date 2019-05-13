open Pipeline_types
(*
module Event : sig

  val get :
    ?f:(Api_js.Websocket.t
        -> (Settings.t, string) result
        -> unit)
    -> unit
    -> (Api_js.Websocket.t, string) result Lwt.t

end

val set : Settings.t -> (unit, Api_js.Http.error) result Lwt.t

val get : unit -> (Settings.t, Api_js.Http.error) result Lwt.t
 *)
