open Board_dektec_dtm3200_types
open Request
open Netlib
open Fsm_common

let ( >>= ) = Lwt.bind

let step
    ~(return : unit -> unit Lwt.t)
    ~(continue : devinfo -> unit Lwt.t)
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (rsp_queue : Cstruct.t cmd Lwt_stream.t)
    (config : config Kv_v.rw) =

  let rec fpga_version () =
    request src sender rsp_queue config (Device FPGA_version)
    >>= function
    | Error _ -> return ()
    | Ok x -> hw_version GList.(x :: [])

  and hw_version acc =
    request src sender rsp_queue config (Device Hardware_version)
    >>= function
    | Error _ -> return ()
    | Ok x -> fw_version GList.(x :: acc)

  and fw_version acc =
    request src sender rsp_queue config (Device Firmware_version)
    >>= function
    | Error _ -> return ()
    | Ok x -> serial GList.(x :: acc)

  and serial acc =
    request src sender rsp_queue config (Device Serial_number)
    >>= function
    | Error _ -> return ()
    | Ok x -> typ GList.(x :: acc)

  and typ acc =
    request src sender rsp_queue config (Device Type)
    >>= function
    | Error _ -> return ()
    | Ok x -> mac_address GList.(x :: acc)

  and mac_address acc =
    request src sender rsp_queue config (Network MAC_address)
    >>= function
    | Error _ -> return ()
    | Ok mac ->
      let devinfo = match acc with
        | typ :: serial :: fw_ver :: hw_ver :: fpga_ver :: [] ->
          { fpga_ver
          ; hw_ver
          ; fw_ver
          ; serial
          ; typ
          ; mac
          } in
      continue devinfo
  in
  fpga_version
