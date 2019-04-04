open Containers
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction.Json
open Api.Redirect
open Common

module WS = struct

  let map ~eq f e = React.E.changes ~eq @@ React.E.map f e

  let state (events:events) _ body sock_data () =
    Api.Socket.handler socket_table sock_data (React.S.changes events.state) Topology.state_to_yojson body

  let ip (events:events) _ body sock_data () =
    let e = map ~eq:Ipaddr.V4.equal (fun c -> c.nw.ip) events.config in
    Api.Socket.handler socket_table sock_data e Ipaddr.V4.to_yojson body

  let mask (events:events) _ body sock_data () =
    let e = map ~eq:Ipaddr.V4.equal (fun c -> c.nw.mask) events.config in
    Api.Socket.handler socket_table sock_data e Ipaddr.V4.to_yojson body

  let gateway (events:events) _ body sock_data () =
    let e = map ~eq:Ipaddr.V4.equal (fun c -> c.nw.gateway) events.config in
    Api.Socket.handler socket_table sock_data e Ipaddr.V4.to_yojson body

  let dhcp (events:events) _ body sock_data () =
    let e = map ~eq:Equal.bool (fun c -> c.nw.dhcp) events.config in
    Api.Socket.handler socket_table sock_data e Json.Bool.to_yojson body

  let mode (events:events) _ body sock_data () =
    let e = map ~eq:equal_nw (fun c -> c.nw) events.config in
    Api.Socket.handler socket_table sock_data e nw_to_yojson body

end

module HTTP = struct

  let set setter _of _to body =
    of_body body >>= fun json ->
    (match _of json with
     | Error e -> Lwt_result.fail @@ of_error_string e
     | Ok ip   -> setter ip >|= (_to %> Result.return))
    >>= respond_result

  let reset       (api:api) _ _ ()    = api.reset () >|= Result.return >>= respond_result_unit
  let set_ip      (api:api) _ body () = set api.set_ip Ipaddr.V4.of_yojson Ipaddr.V4.to_yojson body
  let set_mask    (api:api) _ body () = set api.set_mask Ipaddr.V4.of_yojson Ipaddr.V4.to_yojson body
  let set_gateway (api:api) _ body () = set api.set_gateway Ipaddr.V4.of_yojson Ipaddr.V4.to_yojson body
  let set_dhcp    (api:api) _ body () = set api.set_dhcp Json.Bool.of_yojson Json.Bool.to_yojson body

  let nw_setter (api:api) (nw:Board_types.nw) =
    let open Lwt.Infix in
    match equal_nw nw (api.get_config ()).nw with
    | true  -> Lwt.return nw
    | false -> api.set_ip nw.ip
               >>= fun ip   -> api.set_mask nw.mask
               >>= fun mask -> api.set_gateway nw.gateway
               >>= fun gw   -> api.set_dhcp nw.dhcp
               >>= fun dhcp -> api.reset ()
               >>= fun _    -> Lwt.return { ip; mask; gateway = gw; dhcp }

  let set_mode (api:api) _ body () =
    set (nw_setter api) nw_of_yojson nw_to_yojson body

  let get_state (events:events) _ _ () =
    React.S.value events.state |> Topology.state_to_yojson
    |> Result.return |> respond_result

  let get_devinfo (api:api) _ _ () =
    api.get_devinfo () |> Json.Option.to_yojson devinfo_to_yojson
    |> Result.return |> respond_result

  let get api getter to_yojson =
    getter (api.get_config ()).nw |> to_yojson |> Result.return |> respond_result

  let get_ip      (api:api) _ _ () = get api (fun x -> x.ip) Ipaddr.V4.to_yojson
  let get_mask    (api:api) _ _ () = get api (fun x -> x.mask) Ipaddr.V4.to_yojson
  let get_gateway (api:api) _ _ () = get api (fun x -> x.gateway) Ipaddr.V4.to_yojson
  let get_dhcp    (api:api) _ _ () = get api (fun x -> x.dhcp) Json.Bool.to_yojson
  let get_mode    (api:api) _ _ () = get api (fun x -> x) nw_to_yojson

  module Archive = struct

    let get_state _limit _compress _from _till _duration _ _ () =
      not_found ()

    let get_mode _limit _from _till _duration _ _ () =
      not_found ()

  end

end

let handler api events =
  let open Uri in
  let open Boards.Board.Api_handler in
  create_dispatcher
    "device"
    [ create_ws_handler ~docstring:"Notifies client when board state changes"
        ~path:Path.Format.("state" @/ empty)
        ~query:Query.empty
        (WS.state events)
    ; create_ws_handler ~docstring:"Notifies client when IP address is changed"
        ~path:Path.Format.("ip" @/ empty)
        ~query:Query.empty
        (WS.ip events)
    ; create_ws_handler ~docstring:"Notifies client when network mask is changed"
        ~path:Path.Format.("mask" @/ empty)
        ~query:Query.empty
        (WS.mask events)
    ; create_ws_handler ~docstring:"Notifies client when gateway is changed"
        ~path:Path.Format.("gateway" @/ empty)
        ~query:Query.empty
        (WS.gateway events)
    ; create_ws_handler ~docstring:"Notifies client when DHCP is enabled/disabled"
        ~path:Path.Format.("dhcp" @/ empty)
        ~query:Query.empty
        (WS.dhcp events)
    ; create_ws_handler ~docstring:"Notifies client when board mode is changed"
        ~path:Path.Format.("mode" @/ empty)
        ~query:Query.empty
        (WS.mode events)
    ]
    [ `POST, [ create_handler ~docstring:"Resets the board"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("reset" @/ empty)
                 ~query:Query.empty
                 (HTTP.reset api)
             ; create_handler ~docstring:"Sets IP address"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("ip" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_ip api)
             ; create_handler ~docstring:"Sets network mask"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("mask" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_mask api)
             ; create_handler ~docstring:"Sets gateway"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("gateway" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_gateway api)
             ; create_handler ~docstring:"Sets DHCP"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("dhcp" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_dhcp api)
             ; create_handler ~docstring:"Sets board mode"
                 ~restrict:[ `Guest ]
                 ~path:Path.Format.("mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.set_mode api)
             ]
    ; `GET,  [ create_handler ~docstring:"Returns current board state"
                 ~path:Path.Format.("state" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_state events)
             ; create_handler ~docstring:"Returns board description"
                 ~path:Path.Format.("info" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_devinfo api)
             ; create_handler ~docstring:"Returns IP address"
                 ~path:Path.Format.("ip" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_ip api)
             ; create_handler ~docstring:"Returns network mask"
                 ~path:Path.Format.("mask" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_mask api)
             ; create_handler ~docstring:"Returns gateway"
                 ~path:Path.Format.("gateway" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_gateway api)
             ; create_handler ~docstring:"Returns DHCP"
                 ~path:Path.Format.("dhcp" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_dhcp api)
             ; create_handler ~docstring:"Returns board mode"
                 ~path:Path.Format.("mode" @/ empty)
                 ~query:Query.empty
                 (HTTP.get_mode api)
             (* Archive *)
             ; create_handler ~docstring:"Returns archived board state"
                 ~path:Path.Format.("state/archive" @/ empty)
                 ~query:Query.[ "limit",    (module Option(Int))
                              ; "compress", (module Option(Bool))
                              ; "from",     (module Option(Time.Show))
                              ; "to",       (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 HTTP.Archive.get_state
             ; create_handler ~docstring:"Returns archived board mode"
                 ~path:Path.Format.("mode/archive" @/ empty)
                 ~query:Query.[ "limit",    (module Option(Int))
                              ; "from",     (module Option(Time.Show))
                              ; "to",       (module Option(Time.Show))
                              ; "duration", (module Option(Time.Relative)) ]
                 HTTP.Archive.get_mode
             ]
    ]
