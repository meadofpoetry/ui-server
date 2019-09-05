open Util_react
   
let (state : Pc_control_types.Software_updates.state S.t), push_state =
  S.create ~eq:Stdlib.(=) `Unchecked

let upg_lock = Lwt_mutex.create ()

let packages = ref @@ Stack.create ()

let src = Logs.Src.create "Software Updates"

let is_upgraded () =
  match S.value state with
  | `Need_reboot -> true
  | _ -> false
             
module Event = struct

  let get_state (_su : Software_updates.t) _user =
    S.changes state
    |> E.map Pc_control_types.Software_updates.state_to_yojson
    |> Lwt.return

end

let add_update_info_timeout (su : Software_updates.t) =
  let ( let* ) = Lwt.bind in
  Option.iter Lwt.cancel su.update_info_tm;
  let t =
    let* () = Lwt_unix.sleep 3600.0 in
    packages := Stack.create ();
    push_state `Unchecked;
    Lwt.return_unit
  in
  su.update_info_tm <- Some t

let cleanup_update_info_timeout (su : Software_updates.t) =
  Option.iter Lwt.cancel su.update_info_tm;
  packages := Stack.create ();
  su.update_info_tm <- None
             
let status_signal f trans =
  let ( let* ) = Lwt.bind in
  let open Pc_control_types.Software_updates in
  let* status = trans#status in
  let* perc   = trans#percentage in
  let status = S.map ~eq:status_equal status_of_int32 status in
  S.l2 ~eq:Unit.equal f status perc
  |> Lwt.return

let package_processor trans =
  let ( let* ) = Lwt.bind in
  let* package = trans#package in
  E.map (fun (_, pkg, _) -> Stack.push pkg !packages) package
  |> Lwt.return

let ui_server_version list =
  (* TODO hardcoded package name so far *)
  let (let*) = Option.bind in
  let package_name = "ui-server" in
  let starts_with pref s =
    try String.iteri (fun i c -> if c != s.[i] then raise Exit) pref;
        true
    with _ -> false
  in
  let* pack = List.find_opt (starts_with package_name) list in
  match String.split_on_char ';' pack with
  | _::ver::_ -> Some ver
  | _ -> failwith "bad version string"

let error_msg event =
  let error_thread = Lwt_react.E.next event in
  fun ~no_msg ~msg ->
  Fun.protect
    ~finally:(fun () -> Lwt.cancel error_thread)
    (fun () -> match Lwt.state error_thread with
               | Return (_,v) -> msg v
               | _ -> no_msg ())

let kept_size = 10

let kept = Hashtbl.create kept_size

let hash = ref 0

let keep ev =
  let key = !hash in
  Hashtbl.add kept key ev;
  hash := (!hash + 1) mod kept_size;
  fun () -> Hashtbl.remove kept key

let check_for_upgrades (su : Software_updates.t) _user _body _env _state =
  let ( let* ) = Lwt.bind in
  Lwt_mutex.with_lock upg_lock (fun () ->
      match is_upgraded () with
      | true -> Lwt.return (`Error "Need reboot")
      | _ ->
         packages := Stack.create ();

         let* trans = su.pk#create_transaction in
         let* finished = trans#finished in
         let waiter, waker = Lwt.task () in
         let unkeep = keep @@ E.map (fun x -> Lwt.wakeup waker x) finished in
         let* error = trans#error_code in
         let* package_ev = package_processor trans in
         let* status =
           status_signal (fun stat perc -> push_state (`Checking (stat, perc))) trans
         in
         let error_msg = error_msg error in
         let finished_thread = waiter in
         let* () = trans#get_updates 0L in
         let* res = finished_thread in
         unkeep ();
         S.stop status;
         E.stop package_ev;
         match res with
         | (1l, _) ->
           Logs.info ~src (fun m ->
               m "check_for_upgrades: info update succeded");
           if Stack.is_empty !packages
           then push_state `Updates_not_avail
           else push_state `Updates_avail;
           add_update_info_timeout su;
           Lwt.return (`Value (Util_json.Int.to_yojson (Stack.length !packages)))
         | _ ->
           let msg = error_msg
               ~msg:(fun s -> s)
               ~no_msg:(fun () -> "Unknown error") in
           Logs.err ~src (fun m ->
               m "check_for_upgrades: \
                  error `%s` during obtaining info" msg);
           push_state `Unchecked;
           Lwt.return (`Error msg))

let do_upgrade (su : Software_updates.t) reboot _user _body _env _state =
  let ( let* ) = Lwt.bind in
  Lwt_mutex.with_lock upg_lock (fun () ->
      match is_upgraded (), Stack.length !packages = 0 with
      | true, _ -> Lwt.return (`Error "Need reboot")
      | _, true -> Lwt.return (`Error "No new packages available")
      | _ ->
        let* trans = su.pk#create_transaction in
        let* finished = trans#finished in
        let waiter, waker = Lwt.task () in
        let unkeep = keep @@ E.map (fun x -> Lwt.wakeup waker x) finished in
        let* error = trans#error_code in
        let* status =
          status_signal (fun stat perc ->
              push_state (`Upgrading (stat, perc))) trans
        in
        let error_msg = error_msg error in
        let package_list = List.of_seq @@ Stack.to_seq !packages in
        let new_version = match ui_server_version package_list with
          | Some v -> v
          | None -> su.current
        in
        Logs.info ~src (fun m ->
            m "upgrading packages: %s"
            @@ String.concat ", " package_list);
        Logs.info ~src (fun m -> m "running upgrade transaction...");
        let* () = trans#update_packages package_list in
        Logs.info ~src (fun m -> m "upgrade transaction succeeded.");
        let* () = su.updated new_version in
        let* res = waiter in
        Logs.info ~src (fun m ->
            m "upgraded: %s" @@ String.concat ", " package_list);
        unkeep ();
        S.stop status;
        match res with
        | (1l, _) ->
          push_state `Need_reboot;
          Logs.info ~src (fun m -> m "upgrade succeded, rebooting...");
          cleanup_update_info_timeout su;
          let* () =
            if Option.value reboot ~default:true
            then Power.reboot ()
            else Lwt.return_unit in
          Lwt.return `Unit
        | _ ->
          push_state `Unchecked;
          let msg = error_msg
              ~msg:(fun s -> s)
              ~no_msg:(fun () -> "Unknown error") in
          Logs.err (fun m -> m "error %s during upgrade" msg);
          Lwt.return (`Error msg))

let get_state (_su : Software_updates.t) _user _body _env _state =
  let open Pc_control_types.Software_updates in
  let current = S.value state in
  Lwt.return (`Value (state_to_yojson current))
