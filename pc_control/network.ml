open Nm

module String_map = Map.Make(String)
module React = Util_react
             
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
  let open React in
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

    let (.%{}) table key  = List.assoc_opt ~eq:String.equal key table
    let (-->) v converter = v >>= converter

    let reverse_int32 x =
      let mask = 0xFFl in
      let (a,b,c,d) = Int32.(((x lsr 24) land mask), ((x lsr 16) land mask), ((x lsr 8) land mask), (x land mask)) in
      Int32.((d lsl 24) + (c lsl 16) + (b lsl 8) + a)

    let unwrap_string = function OBus_value.V.Basic OBus_value.V.String s -> Some s | _ -> None

    let unwrap_int = function OBus_value.V.Basic OBus_value.V.Int32 i -> Some (Int32.to_int i) | _ -> None

    let unwrap_bool = function OBus_value.V.Basic OBus_value.V.Boolean b -> Some b | _ -> None

    let unwrap_address_list v =
      let unwrap' = function
        |  OBus_value.V.Array ((OBus_value.T.Basic OBus_value.T.Uint32), ilst) -> begin
            match ilst with
            | (OBus_value.V.Basic OBus_value.V.Uint32 adds)::(OBus_value.V.Basic OBus_value.V.Uint32 mask)::_ ->
               Some (Ipaddr.V4.of_int32 @@ reverse_int32 adds, mask)
            | _ -> None
          end
        | _ -> None
      in
      match v with
      | OBus_value.V.Array (OBus_value.T.Array (OBus_value.T.Basic OBus_value.T.Uint32), ars) -> 
         Some (List.filter_map unwrap' ars)
      | _ -> None
                                                                                        
    let unwrap_ip_list = function
      | OBus_value.V.Array (OBus_value.T.Basic OBus_value.T.Uint32, lst) ->
         Some (List.filter_map
                 (function OBus_value.V.Basic OBus_value.V.Uint32 i -> Some (Ipaddr.V4.of_int32 @@ reverse_int32 i)
                         | _ -> None)
                 lst)
      | _ -> None

    let unwrap_bytes = function
      | OBus_value.V.Array (OBus_value.T.Basic OBus_value.T.Byte, lst) ->
         let s = String.of_list (List.filter_map (function OBus_value.V.Basic OBus_value.V.Byte c -> Some c
                                                         | _ -> None) lst)
         in
         Some (Bytes.of_string s)
      | OBus_value.V.Byte_array s -> Some (Bytes.of_string s)
      | _ -> None

    let unwrap_autoconnect (flag, prior) = match (flag --> unwrap_bool), (prior --> unwrap_int) with
      | Some false, _ -> Some False
      | _, Some p     -> Some (True p)
      | _ -> None

    let of_dbus d =
      let open Option.Infix in

      let of_eth opts =
        opts.%{"mac-address"} --> unwrap_bytes >>= fun x -> Result.to_opt @@ Macaddr.of_bytes @@ Bytes.to_string x
        >>= fun mac_address -> Some { mac_address }
      in
      
      let of_conn opts =
        opts.%{"id"} --> unwrap_string
        >>= fun id ->
        opts.%{"uuid"} --> unwrap_string
        >>= fun uuid ->
        (opts.%{"autoconnect"}, opts.%{"autoconnect-priority"})
        |> unwrap_autoconnect
        <+> (Some (True 0))
        >>= fun autoconnect ->
        Some { autoconnect; id; uuid }
      in
      
      let of_ipv4 opts =
        let gateway =
          opts.%{"gateway"} --> unwrap_string
          >>= fun x -> Result.to_opt @@ Ipaddr.V4.of_string x
        in
        let static =
          opts.%{"routes"} --> unwrap_address_list |> function Some l -> l | None -> []
        in
        let routes = { gateway; static } in
        opts.%{"addresses"} --> unwrap_address_list >>= List.head_opt
        >>= fun address ->
        opts.%{"dns"} --> unwrap_ip_list
        >>= fun dns ->
        opts.%{"method"} --> unwrap_string >>= meth_of_string
        >>= fun meth ->
        Some { address; routes; dns; meth }
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

    let wrap_bytes x = OBus_value.V.byte_array @@ Bytes.to_string x

    let wrap_string x = OBus_value.V.basic_string x

    let wrap_bool x = OBus_value.V.basic_boolean x

    let wrap_int32 x = OBus_value.V.basic_int32 @@ Int32.of_int x

    let wrap_ip_string x = OBus_value.V.basic_string @@ Ipaddr.V4.to_string x

    let wrap_dns_list x =
      List.map (fun dns -> OBus_value.V.basic_uint32 @@ reverse_int32 @@ Ipaddr.V4.to_int32 dns) x
      |> fun a -> (OBus_value.V.array (OBus_value.T.Basic OBus_value.T.Uint32) a)

    let wrap_address_data_list x =
      let open OBus_value in
      List.map (fun (addr, mask) ->
          (V.dict T.String T.Variant
             [ V.string "address", V.variant @@ V.basic_string @@ Ipaddr.V4.to_string addr
             ; V.string "prefix", V.variant @@ V.basic_uint32 mask ]))
        x
      |> V.array (T.Dict (T.String, T.Variant))

    let wrap_route_data_list x =
      let open OBus_value in
      List.map (fun (addr, mask) ->
          (V.dict T.String T.Variant
             [ V.string "dest", V.variant @@ V.basic_string @@ Ipaddr.V4.to_string addr
             ; V.string "prefix", V.variant @@ V.basic_uint32 mask ]))
        x
      |> V.array (T.Dict (T.String, T.Variant))
                
    let to_dbus c =
      let eth  = [ "mac-address", wrap_bytes @@ Bytes.of_string @@ Macaddr.to_bytes c.ethernet.mac_address] in
      let conn = [ "id",   wrap_string c.connection.id
                 ; "uuid", wrap_string c.connection.uuid ]
                 @ match c.connection.autoconnect with
                   | False  -> [ "autoconnect",          wrap_bool false ]
                   | True p -> [ "autoconnect-priority", wrap_int32 p ]
      in
      let ipv4 =
        let address_data = wrap_address_data_list [c.ipv4.address] in
        let routes    = match c.ipv4.routes.gateway with
          | None   -> `Static  (wrap_route_data_list c.ipv4.routes.static)
          | Some x -> `Gateway (wrap_ip_string x)
        in
        let dns  = wrap_dns_list c.ipv4.dns in
        let meth = wrap_string @@ meth_to_string c.ipv4.meth in
        [ "address-data", address_data
        ; "dns", dns
        ; "method", meth ]
        @ match routes with
          | `Static rd -> [ "route-data", rd ]
          | `Gateway g -> [ "gateway", g ]
      in
      [ "802-3-ethernet", eth; "connection", conn; "ipv4", ipv4]
      
  end
                
  module Device = struct
    include OBus_proxy.Private
    open Nm_device_interfaces.Org_freedesktop_NetworkManager_Device
       
    let make bus path =
      OBus_proxy.make
        ~peer:(OBus_peer.make ~connection:bus ~name:"org.freedesktop.NetworkManager")
        ~path
      
    let type_ethernet = 1l
                      
    let type_prop proxy =
      OBus_property.make ~monitor p_DeviceType proxy

    let interface_prop proxy =
      OBus_property.make ~monitor p_Interface proxy

    let connection_prop proxy =
      OBus_property.make ~monitor p_ActiveConnection proxy

    let get_applied_connection proxy () =
      OBus_method.call m_GetAppliedConnection proxy 0l
      >>= fun (set,_) -> Lwt.return set

    let reapply proxy s v =
      OBus_method.call m_Reapply proxy (s, v, 0l)

  end

  module Connection = struct
    include OBus_proxy.Private
    open Nm_connection_interfaces.Org_freedesktop_NetworkManager_Connection_Active
       
    let make bus path =
      OBus_proxy.make
        ~peer:(OBus_peer.make ~connection:bus ~name:"org.freedesktop.NetworkManager")
        ~path

    let settings_prop proxy =
      OBus_property.make ~monitor p_Connection proxy
      
  end
                    
  module Settings = struct
    include OBus_proxy.Private
    open Nm_settings_interfaces.Org_freedesktop_NetworkManager_Settings_Connection

    let make bus path =
      OBus_proxy.make
        ~peer:(OBus_peer.make ~connection:bus ~name:"org.freedesktop.NetworkManager")
        ~path

    let get_settings proxy =
      OBus_method.call m_GetSettings proxy

    let update proxy s =
      OBus_method.call m_Update proxy s

    let save proxy () =
      OBus_method.call m_Save proxy ()
      
  end

  include OBus_proxy.Private
  open Nm_common_interfaces.Org_freedesktop_NetworkManager

  let make bus =
    OBus_proxy.make
      ~peer:(OBus_peer.make ~connection:bus ~name:"org.freedesktop.NetworkManager")
      ~path:[ "org"; "freedesktop"; "NetworkManager" ]
    
  let devices_prop proxy =
    OBus_property.make ~monitor p_AllDevices proxy

  let connections_prop proxy =
    OBus_property.make ~monitor p_ActiveConnections proxy

  let activate_connection proxy (dev : Device.t) (settings : Settings.t) =
    OBus_method.call m_ActivateConnection proxy (OBus_proxy.path settings, OBus_proxy.path dev, OBus_path.empty)
    >>= fun _ -> Lwt.return_unit
               
  let add_and_activate proxy =
    OBus_method.call m_AddAndActivateConnection proxy
    
end

let debug_print_settings conf =
  Lwt_io.printf "Value: %s\n" (List.fold_left (fun acc (n, v) ->
       Printf.sprintf "%s\n%s: { %s }" acc n (List.fold_left (fun acc (n, v) ->
                   Printf.sprintf "%s\n\t%s: %s" acc n (OBus_value.V.string_of_single v)) "" v))
                 "" conf) |> ignore

type apply_error = [ `Network_conf_apply of exn]

type get_conf_error = [ `No_network_config of string]
  
class virtual eth_connection = object
          method virtual get_config
                         : 'a. unit -> (Network_config.t,[>get_conf_error] as 'a) Lwt_result.t
          method virtual apply
                         : 'a. Network_config.t -> (unit,[>apply_error] as 'a) Lwt_result.t
        end
                       
  
let eth_connection ?defaults bus nm name device
    : (eth_connection, [> `Nm_no_connection of string | `Nm_no_settings of string]) Lwt_result.t =
  let (>>=?) = Lwt_result.bind in
  
  OBus_property.get @@ Nm.Device.connection_prop device
  >>= fun conn_path ->

  (if OBus_path.compare OBus_path.empty conn_path <> 0
   then Lwt.return conn_path
   else (let defaults = match defaults with
           | None -> failwith ("no defaults for " ^ name)
           | Some d -> Nm.Config.to_dbus d
         in 
         Nm.Device.reapply device defaults 1L
         >>= fun () -> OBus_property.get @@ Nm.Device.connection_prop device))

  >>= fun conn_path ->

  begin
    try Lwt.return_ok @@ Nm.Connection.make bus conn_path
    with _ -> Lwt.return_error (`Nm_no_connection name)
  end
  >>=? fun connection ->

  OBus_property.get @@ Nm.Connection.settings_prop connection
  >>= fun settings_path ->

  begin
    try Lwt.return_ok @@ Nm.Connection.make bus settings_path
    with _ -> Lwt.return_error (`Nm_no_settings name)
  end
  >>=? fun settings ->

  Lwt.return_ok (object
        
        val nm       = nm
        val name     = name
        val device   = device
        val settings = settings

        method get_config () =
          Nm.Device.get_applied_connection device ()
          >>= fun conf ->
          match Nm.Config.of_dbus conf with
          | None -> Lwt.return_error (`No_network_config name)
          | Some conf -> Lwt.return_ok conf
                       
        method apply new_sets =
          Lwt.catch (fun () ->
              Nm.Settings.update settings (Nm.Config.to_dbus new_sets)
              >>= Nm.Settings.save settings
              >>= fun () ->
              Nm.activate_connection nm device settings
              >>= Lwt.return_ok)
            (fun e -> Lwt.return_error (`Network_conf_apply e))
          
      end)
(*
module Conf = Storage.Config.Make(Network_settings)
 *)
module Net_options = Kv_v.RW (Network_config)
  
type t = { intern : eth_connection
         ; extern : (eth_connection * Net_options.t) option
         }
            
let create (kv : Kv.RW.t) =
  let (>>=?) = Lwt_result.bind in
  let settings_path = ["pc"; "network_settings"]
  and internal_options = ["pc"; "network"; "internal"]
  in
  Kv.RW.parse Network_settings.of_string kv settings_path
  >>=? fun config ->

  OBus_bus.system ()
  >>= fun bus ->
  
  let nm_proxy = Nm.make bus in

  OBus_property.get @@ Nm.devices_prop nm_proxy
  >>= fun device_paths ->

  List.map (Nm.Device.make bus) device_paths
  |> (fun x -> try List.map (fun proxy ->
                       (OBus_property.get @@ Nm.Device.type_prop proxy)
                       >>= fun typ ->
                       (OBus_property.get @@ Nm.Device.interface_prop proxy)
                       >>= fun iface ->
                       Lwt.return (typ, iface, proxy)) x
               with _ -> [])
  |> Lwt_list.map_p (fun x -> x)
  >>= fun devices ->

  (* The Internal ETH *)
  begin match List.find_opt (fun (typ, name, _proxy) ->
                  Int32.equal Nm.Device.type_ethernet typ
                  && String.equal config.intern.interface name)
                devices
  with None -> Lwt.return_error (`No_network_device "internal")
     | Some (_,name,proxy) -> eth_connection bus nm_proxy name proxy
  end
  >>=? fun interior ->

  (* The External ETH *)
  begin match
    Option.(>>=) config.extern (fun conf ->
        List.find_opt (fun (typ, name, _proxy) ->
            Int32.equal Nm.Device.type_ethernet typ
            && String.equal conf.interface name)
          devices)
  with None -> Lwt.return_error (`No_network_device "external")
     | Some (_,name,proxy) -> eth_connection bus nm_proxy name proxy
  end
  >>= fun exterior -> 

  (* Apply exterior settings *)
  begin match exterior with
  | Error _ -> Lwt.return_ok (None)
  | Ok v  ->
     v#get_config ()
     >>=? fun applied ->
     let default = match config.extern with
       | None   -> applied
       | Some c -> Option.get_or (Network_settings.apply applied c) ~default:applied
     in
     Net_options.create ~default kv internal_options
     >>=? fun options ->
     Lwt.return_ok (Some (v, options))
  end
  
  >>=? fun exterior ->
  
  (* push settings *)
  begin
    interior#get_config ()
    >>=? fun conf ->
    match Network_settings.apply conf config.intern with
    | None -> begin
        match conf.connection.autoconnect with
        | True _ -> Lwt.return_ok ()
        | _ -> let conf = { conf with
                            connection = { conf.connection with autoconnect = True (-999) } }
               in interior#apply conf
      end
    | Some new_conf ->
        let new_conf = { new_conf with
                         connection = { new_conf.connection with autoconnect = True (-999) } }
        in interior#apply new_conf
  end
  >>=? fun () ->
  
  (* external iface settings *)
  begin
    match exterior with
    | None -> Lwt.return_ok ()
    | Some (exterior, external_opts) ->
       exterior#get_config ()
       >>=? fun conf ->
       external_opts#get
       >>= fun old_conf ->
       if Network_config.equal conf old_conf
       then Lwt.return_ok ()
       else exterior#apply old_conf
  end
  >>=? fun () ->

  Lwt.return_ok { intern = interior
                ; extern = exterior
    }

let get_ext_settings net =
  match net.extern with
  | None -> Lwt.return_error (`No_network_device "external")
  | Some (_ext, ext_opts) ->
     ext_opts#get >>= Lwt.return_ok (* ext#get_config () *)

let apply_ext_settings (net : t) sets =
  match net.extern with
  | None -> Lwt.return_error (`No_network_device "external")
  | Some (ext, ext_opts) ->
     ext_opts#get
     >>= fun opts ->
     if Network_config.equal sets opts
     then Lwt.return_ok ()
     else (ext_opts#set sets >>= fun () -> ext#apply sets)
          
let finalize _ = ()
