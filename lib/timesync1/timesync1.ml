open Timesync1_interfaces.Org_freedesktop_timesync1_Manager

type t =
  < ntp_server : string Lwt.t ; ntp_server_ip : Netlib.Ipaddr.V4.t Lwt.t >

let make () =
  let ( let* ) = Lwt.bind in
  let* system_bus = OBus_bus.system () in
  let proxy =
    OBus_proxy.make
      ~peer:
        (OBus_peer.make ~connection:system_bus
           ~name:"org.freedesktop.timesync1")
      ~path:[ "org"; "freedesktop"; "timesync1" ]
  in
  Lwt.return
    (object
       method ntp_server =
         let prop = OBus_property.make p_ServerName proxy in
         OBus_property.get prop

       method ntp_server_ip =
         let prop = OBus_property.make p_ServerAddress proxy in
         let* _, arr = OBus_property.get prop in
         Lwt.return (Netlib.Ipaddr.V4.of_octets_exn arr)
    end)
