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

  module Device = struct
    include OBus_proxy.Private
    open Nm_device_interfaces.Org_freedesktop_NetworkManager_Device
    
    let make bus path =
      OBus_proxy.make
      ~peer:(OBus_peer.make bus "org.freedesktop.NetworkManager")
      ~path
      
    let type_ethernet = 1l
      
    let type_prop proxy =
      OBus_property.make ~monitor p_DeviceType proxy

    let interface_prop proxy =
      OBus_property.make ~monitor p_Interface proxy

    let connection_prop proxy =
      OBus_property.make ~monitor p_ActiveConnection proxy

    let get_applied_connection proxy =
      OBus_method.call m_GetAppliedConnection proxy 0l

  end

  module Connection = struct
    include OBus_proxy.Private
    open Nm_connection_interfaces.Org_freedesktop_NetworkManager_Connection_Active
          
    let make bus path =
      OBus_proxy.make
      ~peer:(OBus_peer.make bus "org.freedesktop.NetworkManager")
      ~path

    let settings_prop proxy =
      OBus_property.make ~monitor p_Connection proxy
      
  end
    
  module Settings = struct
    include OBus_proxy.Private
    open Nm_settings_interfaces.Org_freedesktop_NetworkManager_Settings_Connection

    let make bus path =
      OBus_proxy.make
      ~peer:(OBus_peer.make bus "org.freedesktop.NetworkManager")
      ~path

    let get_settings proxy =
      OBus_method.call m_GetSettings proxy

    let update proxy =
      OBus_method.call m_Update proxy
      
  end

  include OBus_proxy.Private
  open Nm_common_interfaces.Org_freedesktop_NetworkManager

  let make bus =
    OBus_proxy.make
      ~peer:(OBus_peer.make bus "org.freedesktop.NetworkManager")
      ~path:[ "org"; "freedesktop"; "NetworkManager" ]
     
  let devices_prop proxy =
    OBus_property.make ~monitor p_AllDevices proxy

  let connections_prop proxy =
    OBus_property.make ~monitor p_ActiveConnections proxy

  let add_and_activate proxy =
    OBus_method.call m_AddAndActivateConnection proxy
                  
end
          
let (|->) v f = f (Lwt_main.run v)
let (%) f g x = f (g x)

type eth = { name     : string
           ; proxy    : Nm.Device.t
           ; settings : Nm.Settings.t
           }

let make_eth bus name proxy =
  let conn_path  = Lwt_main.run @@ OBus_property.get @@ Nm.Device.connection_prop proxy in
  let connection =
    if OBus_path.compare OBus_path.empty conn_path <> 0
    then
      try Nm.Connection.make bus conn_path
      with _ -> failwith "no active connection avail"
    else (* Create a new connection *)
      let nm = Nm.make bus in
      Lwt_main.run (
          Nm.add_and_activate nm ([], (OBus_proxy.path proxy), OBus_path.empty)
          >>= fun (o, conn_path) ->
          Lwt_io.printf "TEST: %s\n" (OBus_path.to_string o) |> ignore;
          Lwt.return (o, conn_path)
          >|= fun (o, conn_path) ->
          try Nm.Connection.make bus conn_path
          with _ -> failwith "no active connection avail"
        )
  in
  let settings =
    OBus_property.get @@ Nm.Connection.settings_prop connection
    >|= (fun set_path ->
      try Nm.Settings.make bus set_path
      with _ -> failwith "no settings avail")
  in
  { name; proxy;
    settings = Lwt_main.run settings
  }

class t internal =
  let bus      = Lwt_main.run @@ OBus_bus.system () in
  let nm_proxy = Nm.make bus in
  let devices  =
    Lwt_main.run (OBus_property.get @@ Nm.devices_prop nm_proxy
                  >>= (fun device_paths ->
          List.map (Nm.Device.make bus) device_paths
          |> List.map (fun proxy ->
                 (OBus_property.get @@ Nm.Device.type_prop proxy) >>= fun typ ->
                 (OBus_property.get @@ Nm.Device.interface_prop proxy) >>= fun iface ->
                 Lwt.return (typ, iface, proxy))
          |> Lwt_list.map_p (fun x -> x)))
    |> List.fold_left (fun (_int, _ext) (typ, name, proxy) ->
           if not @@ Int32.equal Nm.Device.type_ethernet typ
           then (_int, _ext)
           else if String.equal internal name
           then (Some (make_eth bus name proxy), _ext)
           else (_int, Some (make_eth bus name proxy)))
         (None, None)
  in
  let interior, exterior = match devices with
    | Some _int, Some _ext -> _int, _ext
    | _, None  -> failwith "no exterior interface found"
    | _        -> failwith "no interface found"
  in
  object
    val interior = interior
    val exterior = exterior
    method get_settings () =
      Nm.Settings.get_settings exterior.settings ()
    method update_settings s =
      Nm.Settings.update exterior.settings s
  end

let test () =
  let c = new t "eno1" in
  c#get_settings ()
  >>= fun conns ->
  Lwt_io.printf "%s\n" (List.fold_left (fun acc (s,v) -> Printf.sprintf "%s\n, %s: { %s }"
                                                           acc s
                                                           (List.fold_left (fun acc (s,v) -> Printf.sprintf "%s; %s : %s" acc s (OBus_value.V.string_of_single v)) "" v))
                          "" conns)
    
  (*
  |-> Nm_settings1.list_connections
  |-> List.map (fun c -> Nm_settings.Connection.get_settings c |-> List.map fst)
   *)
