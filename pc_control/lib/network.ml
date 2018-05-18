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

  module Config = struct
    include Network_config

    let reverse_int32 x =
      let mask = 0xFFl in
      let (a,b,c,d) = Int32.(((x lsr 24) land mask), ((x lsr 16) land mask), ((x lsr 8) land mask), (x land mask)) in
      Int32.((d lsl 24) + (c lsl 16) + (b lsl 8) + a)
                      
    let of_dbus d =
      let open Option.Infix in
      let open OBus_value in
      let (.%{}) table key  = List.assoc_opt ~eq:String.equal key table in
      let (-->) v converter = v >>= converter in
      
      let of_eth opts =
        opts.%{"mac-address"}
        --> (function
             | V.Array (T.Basic T.Byte, lst) ->
                let s = String.of_list (List.filter_map (function V.Basic V.Byte c -> Some c
                                                                | _ -> None) lst)
                in
                Some (Bytes.of_string s)
             | V.Byte_array s -> Some (Bytes.of_string s)
             | x -> None)
        >>= fun mac_address -> Some { mac_address }
      in
      let of_conn opts =
        opts.%{"id"}
        --> (function V.Basic V.String s -> Some s | _ -> None)
        >>= fun id ->
        opts.%{"uuid"}
        --> (function V.Basic V.String s -> Some s | _ -> None)
        >>= fun uuid -> 
        opts.%{"autoconnect-priority"}
        --> (function
             | V.Basic V.Int32 i -> Some (Int32.to_int i)
             | x -> None)
        |> (function
            | Some p -> Some (True p)
            | None   -> begin
                opts.%{"autoconnect"}
                --> (function
                     | V.Basic V.Boolean v -> Some v
                     | _ -> None)
                |> (function Some false -> Some False | _ -> None)
              end)
        >>= fun autoconnect ->
        Some { autoconnect; id; uuid }
      in
      let of_ipv4 opts =
        opts.%{"addresses"}
        --> (function
             | V.Array (T.Array (T.Basic T.Uint32), ars) -> begin
                 match ars with
                 | V.Array ((T.Basic T.Uint32), ilst)::_ -> begin
                     match ilst with
                     | (V.Basic V.Uint32 adds)::(V.Basic V.Uint32 mask)::_ ->
                        Some (Ipaddr.V4.of_int32 @@ reverse_int32 adds, mask)
                     | _ -> None
                   end
                 | _ -> None
               end 
             | _ -> None)                            
        >>= fun (address, mask) ->
        opts.%{"gateway"}
        --> (function V.Basic V.String s -> Some s | _ -> None)
        >>= Ipaddr.V4.of_string
        >>= fun gateway ->
        opts.%{"dns"}
        --> (function
             | V.Array (T.Basic T.Uint32, lst) -> Some (List.filter_map
                                                          (function V.Basic V.Uint32 i -> Some (Ipaddr.V4.of_int32 @@ reverse_int32 i)
                                                                  | _ -> None)
                                                          lst)
             | _ -> None)
        >>= fun dns ->
        opts.%{"method"}
        --> (function
             | V.Basic V.String m -> meth_of_string m
             | _ -> None)
        >>= fun meth ->
        Some { address; mask; gateway; dns; meth }
      in
      List.fold_left (fun (eth, conn, ipv4) (name, opts) ->
          match name with
          | "802-3-ethernet" -> (of_eth opts, conn, ipv4)
          | "connection"     -> (eth, of_conn opts, ipv4)
          | "ipv4"           -> (eth, conn, of_ipv4 opts)
          | _                -> (eth, conn, ipv4))
        (None, None, None) d
      |> function
        | (Some ethernet, Some connection, Some ipv4) -> Some { ethernet; connection; ipv4; ipv6 = (); proxy = () }
        | _ -> None
                      
    let to_dbus c =
      let open OBus_value in
      let eth  = [ "mac-address", V.byte_array @@ Bytes.to_string c.ethernet.mac_address] in
      let conn = [ "id", V.basic_string c.connection.id
                 ; "uuid", V.basic_string c.connection.uuid ]
                 @ match c.connection.autoconnect with
                   | False  -> [ "autoconnect", V.basic_boolean false ]
                   | True p -> [ "autoconnect-priority", V.basic_int32 @@ Int32.of_int p ]
      in
      let ipv4 =
        let addresses = [ V.basic_uint32 @@ reverse_int32 @@ Ipaddr.V4.to_int32 c.ipv4.address
                        ; V.basic_uint32 c.ipv4.mask
                        ; V.basic_uint32 @@ reverse_int32 @@ Ipaddr.V4.to_int32 c.ipv4.gateway
                        ]
        in
        let address_data = [ V.string "address", V.variant @@ V.basic_string @@ Ipaddr.V4.to_string c.ipv4.gateway
                           ; V.string "prefix", V.variant @@ V.basic_uint32 c.ipv4.mask
                           ]
        in
        let dns = List.map (fun dns -> V.basic_uint32 @@ reverse_int32 @@ Ipaddr.V4.to_int32 dns) c.ipv4.dns
        in
        [ "addresses", V.array (T.Array (T.Basic T.Uint32)) [(V.array (T.Basic T.Uint32) addresses)]
        ; "address-data", V.array (T.Dict (T.String, T.Variant)) [V.dict T.String T.Variant address_data]
        ; "gateway", V.basic_string @@ Ipaddr.V4.to_string c.ipv4.gateway
        ; "dns", V.array (T.Basic T.Uint32) dns
        ; "method", V.basic_string @@ meth_to_string c.ipv4.meth
        ]
      in
      [ "802-3-ethernet", eth; "connection", conn; "ipv4", ipv4]
      
  end
      
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

    let reapply proxy s v =
      OBus_method.call m_Reapply proxy (s, v, 0l)

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

type eth_object = { name     : string
                  ; proxy    : Nm.Device.t
                  ; settings : Nm.Settings.t
                  }
               
let initialize_eth_object ?defaults bus name proxy =
  let conn_path  = Lwt_main.run @@ OBus_property.get @@ Nm.Device.connection_prop proxy in
  let connection =
    let conn_path =
      if OBus_path.compare OBus_path.empty conn_path <> 0
      then conn_path
      else match defaults with
           | None   -> failwith ("no defaults for " ^ name)
           | Some s -> Lwt_main.run begin
                           Nm.Device.reapply proxy s 1L
                           >>= fun () -> OBus_property.get @@ Nm.Device.connection_prop proxy
                         end
    in
    try Nm.Connection.make bus conn_path
    with _ -> failwith "no active connection avail"
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

let debug_print_settings conf =
  Lwt_io.printf "Value: %s\n" (List.fold_left (fun acc (n, v) ->
                                   Printf.sprintf "%s\n%s: { %s }" acc n (List.fold_left (fun acc (n, v) ->
                                                                              Printf.sprintf "%s\n\t%s: %s" acc n (OBus_value.V.string_of_single v)) "" v))
                                 "" conf) |> ignore;

module Conf = Storage.Config.Make(Network_settings)
module Opts = Storage.Options.Make(Network_config.Options)
              
class t (internal_conf : Network_settings.t) (external_opts : Network_config.t option Storage.Options.storage) =
  let bus      = Lwt_main.run @@ OBus_bus.system () in
  let nm_proxy = Nm.make bus in
  let devices  =
    Lwt_main.run (OBus_property.get @@ Nm.devices_prop nm_proxy
                  >>= (fun device_paths ->
          List.map (Nm.Device.make bus) device_paths
          |> (fun x -> try List.map (fun proxy ->
                               (OBus_property.get @@ Nm.Device.type_prop proxy) >>= fun typ ->
                               (OBus_property.get @@ Nm.Device.interface_prop proxy) >>= fun iface ->
                               Lwt.return (typ, iface, proxy)) x
              with _ -> [])
          |> Lwt_list.map_p (fun x -> x)))
  in
  let interior = Option.(List.find_opt (fun (typ, name, proxy) ->
                             Int32.equal Nm.Device.type_ethernet typ
                             && String.equal internal_conf.interface name)
                           devices
                         >>= fun (_,name,proxy) ->
                         Some (initialize_eth_object bus name proxy))
                 |> function Some obj -> obj | None -> failwith "no internal interface was found"
  in
  let exterior = Option.(List.find_opt (fun (typ, name, proxy) ->
                             Int32.equal Nm.Device.type_ethernet typ
                             && not (String.equal internal_conf.interface name))
                           devices
                         >>= fun (_,name,proxy) ->
                         Some (initialize_eth_object ?defaults:(Option.map Nm.Config.to_dbus external_opts#get) bus name proxy))
                 |> function Some obj -> obj | None -> failwith "no external interface was found"
  in
  (* push settings *)
  let () =
    Lwt_main.run begin
        Nm.Settings.get_settings interior.settings ()
        >>= fun conf ->
        match Nm.Config.of_dbus conf with
        | None -> Lwt.fail_with "internal config parsing failure"
        | Some conf ->
           match Network_settings.apply conf internal_conf with
           | None -> Lwt.return_unit
           | Some new_conf -> Nm.Settings.update interior.settings (Nm.Config.to_dbus new_conf)
      end
  in
  (* external iface settings *)
  let () =
    Lwt_main.run begin
        Nm.Settings.get_settings exterior.settings ()
        >>= fun conf ->
        match Nm.Config.of_dbus conf with
        | None -> Lwt.fail_with "external config parsing failure"
        | Some conf as c ->
           match external_opts#get with
           | None -> external_opts#store c; Lwt.return_unit
           | Some old_conf ->
              if Network_config.equal conf old_conf
              then Lwt.return_unit
              else Nm.Settings.update exterior.settings (Nm.Config.to_dbus old_conf)
      end
  in
  object
    val interior = interior
    val exterior = exterior
    method get_settings () =
      match external_opts#get with (* consider direct dbus call *)
      | Some _ as conf -> Lwt.return conf
      | None ->
         Nm.Settings.get_settings interior.settings ()
         >|= fun conf ->
         match Nm.Config.of_dbus conf with
         | None           -> None
         | Some _ as conf -> external_opts#store conf; conf

    method update_settings s =
      let conf = Nm.Config.to_dbus s in
      Nm.Settings.update exterior.settings conf
      >>= fun () ->
      external_opts#store (Some s);
      Lwt.return_unit
  end

let create config =
  try let cfg  = Conf.get config in
      let stor = Storage.Options.Conf.get config in
      let opts = Opts.create stor.config_dir ["network";"external"] in
      Ok (new t cfg opts)
  with e -> Error (Printexc.to_string e)

let finalize _ = ()
              
  (*
  >>= fun conns ->
  Lwt_io.printf "%s\n" (List.fold_left (fun acc (s,v) -> Printf.sprintf "%s\n, %s: { %s }"
                                                           acc s
                                                           (List.fold_left (fun acc (s,v) -> Printf.sprintf "%s; %s : %s" acc s (OBus_value.V.string_of_single v)) "" v))
                          "" conns)
   *)
  (*
  |-> Nm_settings1.list_connections
  |-> List.map (fun c -> Nm_settings.Connection.get_settings c |-> List.map fst)
   *)
