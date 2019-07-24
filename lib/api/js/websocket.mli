type 'a t

include module type of Websocket_intf

val close_socket : ?code:int -> ?reason:string -> 'a t -> unit

module Make(Body : Api.BODY)(Msg : Api.WS_BODY with type t := Body.t)
  : WS with type body := Body.t
        and type t = Body.t t

module JSON : WS with type body := Yojson.Safe.t
                  and type t = Yojson.Safe.t t
