open Board_dektec_dtm3200_types
open Request
open Netlib
open Fsm_common

let ( >>= ) = Lwt.bind

let step
    ~(return : Request.error -> unit Lwt.t)
    ~(continue : devinfo -> unit Lwt.t)
    (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (rsp_queue : Cstruct.t cmd Lwt_stream.t)
    (config : config Kv_v.rw) =

  let ( >>=? ) x f =
    x >>= function
    | Error e -> return e
    | Ok x -> f x in

  let rec fpga_version () =
    request src sender rsp_queue config (Device FPGA_version)
    >>=? fun (x : int) -> hw_version GList.(x :: [])

  and hw_version acc =
    request src sender rsp_queue config (Device Hardware_version)
    >>=? fun (x : int) -> fw_version GList.(x :: acc)

  and fw_version acc =
    request src sender rsp_queue config (Device Firmware_version)
    >>=? fun (x : int) -> serial GList.(x :: acc)

  and serial acc =
    request src sender rsp_queue config (Device Serial_number)
    >>=? fun (x : int) -> typ GList.(x :: acc)

  and typ acc =
    request src sender rsp_queue config (Device Type)
    >>=? fun (x : int) -> mac_address GList.(x :: acc)

  and mac_address acc =
    request src sender rsp_queue config (Network MAC_address)
    >>=? fun (mac : Macaddr.t) ->
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
