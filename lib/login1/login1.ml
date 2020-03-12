open Login1_interfaces.Org_freedesktop_login1_Manager

let poweroff () =
  let ( let* ) = Lwt.bind in
  let* system_bus = OBus_bus.system () in
  let proxy =
    OBus_proxy.make
      ~peer:
        (OBus_peer.make ~connection:system_bus ~name:"org.freedesktop.login1")
      ~path:[ "org"; "freedesktop"; "login1" ]
  in
  OBus_method.call m_PowerOff proxy true

let reboot () =
  let ( let* ) = Lwt.bind in
  let* system_bus = OBus_bus.system () in
  let proxy =
    OBus_proxy.make
      ~peer:
        (OBus_peer.make ~connection:system_bus ~name:"org.freedesktop.login1")
      ~path:[ "org"; "freedesktop"; "login1" ]
  in
  OBus_method.call m_Reboot proxy true
