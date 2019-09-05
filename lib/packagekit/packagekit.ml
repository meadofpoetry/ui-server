
module PK_iface = Packagekit_interfaces

module PK_trans_iface = Packagekit_transaction_interfaces

let ( let* ) = Lwt.bind

(* TODO add Lwt_switch or ensure that resources are being freed properly *)
              
class transaction obj = object

  val obj = obj

  method cancel =
    OBus_method.call PK_trans_iface.Org_freedesktop_PackageKit_Transaction.m_Cancel obj ()

  method get_updates flags =
    OBus_method.call PK_trans_iface.Org_freedesktop_PackageKit_Transaction.m_GetUpdates obj flags

  method update_packages lst =
    OBus_method.call PK_trans_iface.Org_freedesktop_PackageKit_Transaction.m_UpdatePackages obj (0L, lst)

  method package =
    OBus_signal.make PK_trans_iface.Org_freedesktop_PackageKit_Transaction.s_Package obj
    |> OBus_signal.connect

  method finished =
    OBus_signal.make PK_trans_iface.Org_freedesktop_PackageKit_Transaction.s_Finished obj
    |> OBus_signal.connect

  method error_code =
    OBus_signal.make PK_trans_iface.Org_freedesktop_PackageKit_Transaction.s_ErrorCode obj
    |> OBus_signal.connect

  method percentage =
    OBus_property.make PK_trans_iface.Org_freedesktop_PackageKit_Transaction.p_Percentage obj
    |> OBus_property.monitor

  method status =
    OBus_property.make PK_trans_iface.Org_freedesktop_PackageKit_Transaction.p_Status obj
    |> OBus_property.monitor
    
end

class t obj = object

  val obj = obj

  method can_authorize s =
    OBus_method.call PK_iface.Org_freedesktop_PackageKit.m_CanAuthorize obj s

  method create_transaction =
    let* path =
      OBus_method.call PK_iface.Org_freedesktop_PackageKit.m_CreateTransaction obj ()
    in
    let trans = OBus_proxy.make
                  ~peer:obj.OBus_proxy.peer
                  ~path:path
    in
    Lwt.return (new transaction trans)

  method get_transaction_list =
    OBus_method.call PK_iface.Org_freedesktop_PackageKit.m_GetTransactionList obj ()
    
  method repo_list_changed =
    OBus_signal.make PK_iface.Org_freedesktop_PackageKit.s_RepoListChanged obj
    |> OBus_signal.connect

  method restart_schedule =
    OBus_signal.make PK_iface.Org_freedesktop_PackageKit.s_RestartSchedule obj
    |> OBus_signal.connect

  method transaction_list_changed =
    OBus_signal.make PK_iface.Org_freedesktop_PackageKit.s_TransactionListChanged obj
    |> OBus_signal.connect

  method updates_changed =
    OBus_signal.make PK_iface.Org_freedesktop_PackageKit.s_UpdatesChanged obj
    |> OBus_signal.connect

end

let system () =
  Lwt.catch
    (fun () -> Lwt_result.ok (OBus_bus.system ()))
    (fun _ -> Lwt_result.fail (`PackageKit "System bus is not available"))

let create () =
  let ( let* ) = Lwt_result.bind in
  let* system_bus = system () in
  let proxy = OBus_proxy.make
                ~peer:(OBus_peer.make
                         ~connection:system_bus
                         ~name:"org.freedesktop.PackageKit")
                ~path:["org"; "freedesktop"; "PackageKit"]
  in
  Lwt_result.return (new t proxy)
