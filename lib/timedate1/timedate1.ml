open Timedate1_interfaces.Org_freedesktop_timedate1

type t = < list_timezones : string list Lwt.t
       ; timezone : string Lwt.t
       ; set_timezone : string -> unit Lwt.t
       ; set_local_rtc : bool -> unit Lwt.t
       ; local_rtc : bool Lwt.t
       ; ntp : bool Lwt.t
       ; set_ntp : bool -> unit Lwt.t
       ; time : Time.t Lwt.t
       ; set_time : Time.t -> unit Lwt.t
       ; local_time : Time.t Lwt.t >

let make () =
  let ( let* ) = Lwt.bind in
  let* system_bus = OBus_bus.system () in
  let proxy =
    OBus_proxy.make
      ~peer:(OBus_peer.make ~connection:system_bus ~name:"org.freedesktop.timedate1")
      ~path:[ "org"; "freedesktop"; "timedate1" ]
  in
  Lwt.return
    (object
      method list_timezones =
        OBus_method.call m_ListTimezones proxy ()

      method timezone =
        let prop = OBus_property.make p_Timezone proxy in
        OBus_property.get prop

      method set_timezone zone =
        OBus_method.call m_SetTimezone proxy (zone, false)

      method set_local_rtc flag =
        OBus_method.call m_SetLocalRTC proxy (flag, false, false)

      method local_rtc =
        let prop = OBus_property.make p_LocalRTC proxy in
        OBus_property.get prop
        
      method ntp =
        let prop = OBus_property.make p_NTP proxy in
        OBus_property.get prop

      method set_ntp flag =
        OBus_method.call m_SetNTP proxy (flag, false)

      method time =
        let prop = OBus_property.make p_TimeUSec proxy in
        let* usec = OBus_property.get prop in
        Lwt.return (Time.Useconds.of_int64 usec)

      method set_time time =
        let v = Time.Useconds.to_int64 time in
        OBus_method.call m_SetTime proxy (v, false, false)

       method local_time =
        let prop = OBus_property.make p_RTCTimeUSec proxy in
        let* usec = OBus_property.get prop in
        Lwt.return (Time.Useconds.of_int64 usec)
    end)
