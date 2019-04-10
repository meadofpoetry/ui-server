open Boards
open Boards.Pools
open Board_dektec_dtm3200_types
open Request
open Netlib

let ( >>= ) = Lwt.bind

let timeout = 3. (* seconds *)

let step ~(address : int)
      ~entry_point
      ~exit_point
      (src : Logs.src)
      (sender : Cstruct.t -> unit Lwt.t)
      (pe : Sm_common.push_events) =

  let make_req req =
    make_msg
      ~timeout:(fun () -> Lwt_unix.timeout timeout)
      ~send:(fun () -> sender @@ Serializer.make_req ~address req)
      ~resolve:(Parser.is_response req)
      () in

  let deserialize acc recvd =
    let recvd = Board.concat_acc acc recvd in
    Parser.deserialize ~address src recvd in

  let wait ~next_step pending log_data pool acc recvd =
    let (name, to_string) = log_data in
    let responses, acc = deserialize acc recvd in
    Pool.apply pool responses;
    Pool._match pool
      ~resolved:(fun _ -> function
        | `Error e ->
           Logs.warn (fun m -> m "detect - error getting %s: %s" name e);
           entry_point ()
        | `Value x ->
           Logs.debug (fun m -> m "detect - got %s: %s" name (to_string x));
           match next_step x with
           | `Next next -> next ()
           | `CC (r, next) ->
              Pool.(send (create [make_req r]))
              >>= fun pool ->
              Lwt.return @@ `Continue (next pool None))
      ~error:(fun _ -> function
        | `Timeout ->
           Logs.warn (fun m ->
               let err = "timeout" in
               m "detect - error getting %s: %s" name err);
           entry_point ())
      ~pending:(fun pool -> Lwt.return @@ `Continue (pending pool acc))
      ~not_sent:(fun _ -> assert false) in

  let rec first_step () =
    let msg = make_req (Device FPGA_version) in
    Pool.(send (create [msg]))
    >>= fun pool ->
    Lwt.return @@ `Continue (get_fpga_ver pool None)

  and get_fpga_ver pool acc recvd =
    wait ~next_step:(fun x ->
        `CC (Device Hardware_version, get_hw_ver GList.(x :: [])))
      get_fpga_ver ("FPGA version", string_of_int) pool acc recvd

  and get_hw_ver racc =
    wait ~next_step:(fun x ->
        `CC (Device Firmware_version, get_fw_ver GList.(x :: racc)))
      (get_hw_ver racc) ("hardware version", string_of_int)

  and get_fw_ver racc =
    wait ~next_step:(fun x ->
        `CC (Device Serial_number, get_serial GList.(x :: racc)))
      (get_fw_ver racc) ("firmware version", string_of_int)

  and get_serial racc =
    wait ~next_step:(fun x ->
        `CC (Device Type, get_type GList.(x :: racc)))
      (get_serial racc) ("serial number", string_of_int)

  and get_type racc =
    wait ~next_step:(fun x ->
        `CC (Network MAC_address, get_mac GList.(x :: racc)))
      (get_type racc) ("device type", string_of_int)

  and get_mac racc =
    wait ~next_step:(fun mac ->
        let devinfo = match racc with
          | typ :: serial :: fw_ver :: hw_ver :: fpga_ver :: [] ->
             { fpga_ver
             ; hw_ver
             ; fw_ver
             ; serial
             ; typ
             ; mac
             } in
        pe.devinfo (Some devinfo);
        pe.state `Init;
        `Next exit_point)
      (get_mac racc) ("MAC address", Macaddr.to_string ?sep:None)

  in first_step
