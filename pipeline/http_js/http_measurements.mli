open Pipeline_types

module Event : sig
  val get_video :
    ?f:(Api_js.Websocket.t
        -> (Qoe_errors.Video_data.t list, string) result
        -> unit)
    -> ?stream:Application_types.Stream.ID.t
    -> ?channel:int
    -> ?pid:int
    -> unit
    -> (Api_js.Websocket.t, string) result Lwt.t

  val get_audio :
    ?f:(Api_js.Websocket.t
        -> (Qoe_errors.Audio_data.t list, string) result
        -> unit)
    -> ?stream:Application_types.Stream.ID.t
    -> ?channel:int
    -> ?pid:int
    -> unit
    -> (Api_js.Websocket.t, string) result Lwt.t
end
