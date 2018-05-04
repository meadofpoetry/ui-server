module String_map = Map.Make(String)

open Containers
open Lwt.Infix

let properties_changed interface =
  let open OBus_value in
  OBus_member.Signal.make
    ~interface
    ~member:"PropertiesChanged"
    ~args:(arg1 (Some "properties", C.dict C.string C.variant))
    ~annotations:[]

let monitor proxy interface switch =
  let open Lwt_react in
  let open OBus_value in
  OBus_signal.connect ~switch
    (OBus_signal.with_context
       (OBus_signal.make (properties_changed interface) proxy))
  >>= fun event ->
  OBus_property.get_all_no_cache proxy interface
  >>= fun (context, dict) ->
  Lwt.return (S.fold_s ~eq:(String_map.equal Equal.poly)
                (fun (map : (OBus_context.t * OBus_value.V.single) String_map.t) (context, updates) ->
                  Lwt.return (OBus_property.update_map context updates map))
                (OBus_property.map_of_list context dict)
                event)
   
module Nm = struct
  open OBus_proxy.Private
  open Nm_common_interfaces.Org_freedesktop_NetworkManager

  let devices_prop proxy =
    OBus_property.make ~monitor p_AllDevices proxy

  let connections_prop proxy =
    OBus_property.make ~monitor p_ActiveConnections proxy
  
end
   
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

    (*
let test () =
  Nm_settings1.connect ()
  |-> Nm_settings1.list_connections
  |-> List.map (fun c -> Nm_settings.Connection.get_settings c |-> List.map fst)
     *)
