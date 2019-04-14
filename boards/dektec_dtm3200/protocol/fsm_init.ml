(* open Boards
 * open Boards.Pools
 * open Board_dektec_dtm3200_types
 * open Netlib
 * open Request
 * 
 * let ( >>= ) = Lwt.bind
 * 
 * let reboot_steps = 7
 * 
 * let timeout = 3. (\* seconds *\)
 * 
 * let step ~(address : int)
 *       ~return
 *       ~continue
 *       (src : Logs.src)
 *       (sender : Cstruct.t -> unit Lwt.t)
 *       (pe : Sm_common.push_events) =
 * 
 *   let (module Logs : Logs.LOG) = Logs.src_log src in
 * 
 *   let make_req req =
 *     make_msg
 *       ~timeout:(fun () -> Lwt_unix.sleep timeout)
 *       ~send:(fun () -> sender @@ Serializer.make_req ~address req)
 *       ~resolve:(Parser.is_response req)
 *       () in
 * 
 *   let wait ~next_step pending log_data pool acc recvd =
 *     let (meth, name, to_string) = log_data in
 *     let responses, acc =
 *       Parser.deserialize ~address src
 *       @@ Board.concat_acc acc recvd in
 *     Pool.apply pool responses;
 *     Pool._match pool
 *       ~resolved:(fun _ -> function
 *         | `Error e ->
 *            let meth = match meth with `Get -> "getting" | `Set -> "setting" in
 *            Logs.warn (fun m -> m "init - error %s %s: %s" meth name e);
 *            return ()
 *         | `Value x ->
 *            let meth = match meth with `Get -> "got" | `Set -> "set" in
 *            Logs.debug (fun m -> m "init - %s %s: %s" meth name (to_string x));
 *            match next_step x with
 *            | `Next next -> next ()
 *            | `CC (r, next) ->
 *               Pool.(send (create [make_req r]))
 *               >>= fun pool ->
 *               Lwt.return @@ `Continue (next pool None))
 *       ~error:(fun _ -> function
 *         | `Timeout ->
 *            let meth = match meth with `Get -> "getting" | `Set -> "setting" in
 *            Logs.warn (fun m -> m "init - error %s %s: timeout" meth name);
 *            return ())
 *       ~pending:(fun pool -> Lwt.return @@ `Continue (pending pool acc))
 *       ~not_sent:(fun _ -> assert false) in
 * 
 *   let rec first_step () =
 *     let req = make_req (Configuration (Mode (`W IP2ASI))) in
 *     Pool.(send (create [req]))
 *     >>= fun pool ->
 *     Lwt.return @@ `Continue (init_mode pool None)
 * 
 *   and detect_device ns steps pool acc recvd =
 *     let responses, acc =
 *       Parser.deserialize ~address src
 *       @@ Board.concat_acc acc recvd in
 *     Pool.apply pool responses;
 *     Pool._match pool
 *       ~error:(fun pool -> function
 *         | `Timeout ->
 *            if steps = 0
 *            then (
 *              Logs.warn (fun m -> m "init - device is not responding, restarting...");
 *              return ())
 *            else (
 *              Lwt_unix.sleep timeout
 *              >>= fun () -> Pool.send pool
 *              >>= fun pool ->
 *              Lwt.return @@ `Continue (detect_device ns (pred steps) pool acc)))
 *       ~pending:(fun pool -> Lwt.return @@ `Continue (detect_device ns steps pool acc))
 *       ~not_sent:(fun _ -> assert false)
 *       ~resolved:(fun pool -> function
 *         | `Error _ ->
 *            Logs.warn (fun m -> m "init - device responded with error, restarting...");
 *            return ()
 *         | `Value x ->
 *            (\* Sleep for a while after detection *\)
 *            Lwt_unix.sleep timeout
 *            >>= fun () ->
 *            match ns with
 *            | `Mode ->
 *               let req = make_req (Configuration (Application (`W Normal))) in
 *               Pool.(send (create [req]))
 *               >>= fun pool ->
 *               Lwt.return @@ `Continue (init_application pool None)
 *            | `Application ->
 *               let req = make_req (Configuration (Volatile_storage (`W FLASH))) in
 *               Pool.(send (create [req]))
 *               >>= fun pool ->
 *               Lwt.return @@ `Continue (init_storage pool None))
 * 
 *   and init_mode pool acc recvd =
 *     wait ~next_step:(fun _ ->
 *         `CC (Device FPGA_version, detect_device `Mode reboot_steps))
 *       init_mode (`Set, "device mode", mode_to_string) pool acc recvd
 * 
 *   and init_application pool acc recvd =
 *     wait ~next_step:(fun _ ->
 *         `CC (Device FPGA_version, detect_device `Application reboot_steps))
 *       init_application (`Set, "application", application_to_string) pool acc recvd
 * 
 *   and init_storage pool acc recvd =
 *     wait ~next_step:(fun _ ->
 *         `CC (Network (IP_address `R), get_ip))
 *       init_storage (`Set, "volatile storage", storage_to_string) pool acc recvd
 * 
 *   and get_ip pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (Network (Subnet_mask `R), get_subnet_mask GList.(x :: [])))
 *       get_ip (`Get, "IP address", Ipaddr.V4.to_string) pool acc recvd
 * 
 *   and get_subnet_mask racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (Network (Gateway `R), get_gateway GList.(x :: racc)))
 *       (get_subnet_mask racc) (`Get, "subnet mask", Ipaddr.V4.to_string)
 *       pool acc recvd
 * 
 *   and get_gateway racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (Network (DHCP `R), get_dhcp GList.(x :: racc)))
 *       (get_gateway racc) (`Get, "gateway", Ipaddr.V4.to_string) pool acc recvd
 * 
 *   and get_dhcp racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive (Enable `R), get_enable GList.(x :: racc)))
 *       (get_dhcp racc) (`Get, "DHCP", string_of_bool) pool acc recvd
 * 
 *   and get_enable racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive (FEC_enable `R), get_fec GList.(x :: racc)))
 *       (get_enable racc) (`Get, "receive enable", string_of_bool) pool acc recvd
 * 
 *   and get_fec racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive (UDP_port `R), get_udp_port GList.(x :: racc)))
 *       (get_fec racc) (`Get, "FEC enable", string_of_bool) pool acc recvd
 * 
 *   and get_udp_port racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive (Multicast_address `R), get_multicast GList.(x :: racc)))
 *       (get_udp_port racc) (`Get, "UDP port", string_of_int) pool acc recvd
 * 
 *   and get_multicast racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive (Addressing_method `R),
 *              get_addressing_method GList.(x :: racc)))
 *       (get_multicast racc) (`Get, "Multicast address", Ipaddr.V4.to_string)
 *       pool acc recvd
 * 
 *   and get_addressing_method racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive (IP_to_output_delay `R), get_delay GList.(x :: racc)))
 *       (get_addressing_method racc) (`Get, "addressing method", meth_to_string)
 *       pool acc recvd
 * 
 *   and get_delay racc pool acc recvd =
 *     wait ~next_step:(fun x ->
 *         `CC (IP_receive (Rate_estimation_mode `R),
 *              get_rate_mode GList.(x :: racc)))
 *       (get_delay racc) (`Get, "IP-to-output delay", string_of_int)
 *       pool acc recvd
 * 
 *   and get_rate_mode racc pool acc recvd =
 *     wait ~next_step:(fun rate_mode ->
 *         let config = match racc with
 *           | delay :: meth :: multicast :: port :: fec :: enable
 *             :: dhcp :: gateway :: mask :: ip :: [] ->
 *              let multicast = match meth with
 *                | Unicast -> None
 *                | Multicast -> Some multicast in
 *              let nw =
 *                { ip
 *                ; mask
 *                ; gateway
 *                ; dhcp
 *                } in
 *              let recv =
 *                { enable
 *                ; fec
 *                ; port
 *                ; multicast
 *                ; delay
 *                ; rate_mode
 *                } in
 *              { nw; ip = recv } in
 *         (\* TODO 2 ways - write config to kv or set differing settings to the board. *\)
 *         ignore config;
 *         Logs.info (fun m -> m "initialization done!");
 *         pe.state `Fine;
 *         `Next continue)
 *       (get_rate_mode racc) (`Get, "Rate estimation mode", rate_mode_to_string)
 *       pool acc recvd
 * 
 *   in first_step *)
