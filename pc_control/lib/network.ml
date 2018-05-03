open Containers
open Lwt.Infix

module Nm_settings1 = struct
  include Nm_settings
        
  let connect () : OBus_proxy.Private.t Lwt.t =
    OBus_bus.system () >>= fun bus ->
    Lwt.return (OBus_proxy.make
                  (OBus_peer.make bus "org.freedesktop.NetworkManager.Settings")
                  [ "org"; "freedesktop"; "NetworkManager"; "Settings" ])
end
   
let (|->) v f = f (Lwt_main.run v)
              
class t internal =
  let x = 42 in
  let filter_eth =
    List.filter (fun d -> Nm_device.device_type d
                          |> OBus_property.get
                          |-> function `Ethernet -> true | _ -> false)
  in
  let filter_ext =
    List.filter (fun d -> Nm_device.interface d
                          |> OBus_property.get
                          |-> (fun n -> not (String.equal internal n)))
  in
  object

    

  end

let test () =
  Nm_settings1.connect ()
  |-> Nm_settings1.list_connections
  |-> List.map (fun c -> Nm_settings.Connection.get_settings c |-> List.map fst)
