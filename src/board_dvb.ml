open Common.Hardware
open Api_handler
open Interaction
open Board_types

module Make (V : VERSION) : BOARD = struct

  type err = Bad_tag_start of int
           | Bad_length of (int * int)
           | Bad_crc of (int * int)
           | Bad_tag_stop of int

  type t = { handlers : (module HANDLER) list }

  let tag_start = 0x55AA
  let tag_stop  = 0xFE

  [@@@ocaml.warning "-32"]

  [%%cenum
   type mode =
     | T2 [@id 1]
     | T  [@id 2]
     | C  [@id 3] [@@uint8_t]]

  [%%cenum
   type bw =
     | Bw8 [@id 1]
     | Bw7 [@id 2]
     | Bw6 [@id 3] [@@uint8_t]]

  [%%cenum
   type dvbc_qam =
     | Qam32
     | Qam64
     | Qam128
     | Qam256 [@@uint8_t]]

  [%%cstruct
   type prefix =
     { tag_start : uint16_t
     ; length    : uint8_t
     ; msg_code  : uint8_t
     } [@@little_endian]]

  [%%cstruct
   type suffix =
     { crc      : uint8_t
     ; tag_stop : uint8_t
     } [@@little_endian]]

  [%%cstruct
   type cmd_devinfo =
     { reset    : uint8_t
     } [@@little_endian]]

  [%%cstruct
   type rsp_devinfo =
     { serial    : uint16_t
     ; hw_ver    : uint8_t
     ; fpga_ver  : uint8_t
     ; soft_ver  : uint8_t
     ; hw_config : uint8_t
     ; rfu       : uint32_t
     } [@@little_endian]]

  [%%cstruct
   type settings =
     { mode       : uint8_t
     ; bw         : uint8_t
     ; hw_present : uint8_t  (* rfu in cmd *)
     ; dvbc_qam   : uint8_t
     ; freq       : uint32_t
     ; plp_id     : uint8_t
     ; lock       : uint8_t  (* rfu in cmd *)
     } [@@little_endian]]

  [%%cstruct
   type rsp_measure =
     { lock    : uint8_t
     ; power   : uint16_t
     ; mer     : uint16_t
     ; ber     : uint32_t
     ; freq    : uint32_t
     ; bitrate : uint32_t
     } [@@little_endian]]

  [%%cstruct
   type cmd_plp_set =
     { plp_id : uint8_t } [@@little_endian]]

  [@@@ocaml.warning "+32"]

  type devinfo =
    { serial   : int
    ; hw_ver   : int
    ; fpga_ver : int
    ; soft_ver : int
    ; asi      : bool
    ; modules  : int list
    }

  type settings =
    { mode : mode
    ; bw   : bw
    ; dvbc_qam : dvbc_qam
    ; freq     : Int32.t
    ; plp_id   : int
    }

  type settings_resp =
    { settings   : settings
    ; hw_present : bool
    ; lock       : bool
    }

  let calc_crc (msg:Cstruct.t) =
    let _,body = Cstruct.split msg (sizeof_prefix - 1) in
    let iter = Cstruct.iter (fun _ -> Some 1)
                            (fun buf -> Cstruct.get_uint8 buf 0)
                            body in
    Cstruct.fold (fun acc el -> el lxor acc) iter 0

  let length (msg:Cstruct.t) =
    (Cstruct.len msg) + 1

  let to_prefix ~length ~msg_code =
    let prefix = Cstruct.create sizeof_prefix in
    let () = set_prefix_tag_start prefix tag_start in
    let () = set_prefix_length prefix length in
    let () = set_prefix_msg_code prefix msg_code in
    prefix

  let to_suffix ~crc =
    let suffix = Cstruct.create sizeof_suffix in
    let () = set_suffix_crc suffix crc in
    let () = set_suffix_tag_stop suffix tag_stop in
    suffix

  let to_msg ~msg_code ~body =
    let prefix = to_prefix ~length:(length body) ~msg_code in
    let msg = Cstruct.append prefix body in
    let suffix  = to_suffix ~crc:(calc_crc msg) in
    Cstruct.append msg suffix |> Cstruct.to_string

  let to_empty_msg ?(length=1) ~msg_code =
    let body = Cstruct.create length in
    to_msg ~msg_code ~body

  let cmd_devinfo reset =
    let body = Cstruct.create sizeof_cmd_devinfo in
    let () = set_cmd_devinfo_reset body (if reset then 0xFF else 0) in
    to_msg ~msg_code:0x10 ~body

  let cmd_settings id settings =
    let body = Cstruct.create sizeof_settings in
    let () = set_settings_mode body (mode_to_int settings.mode) in
    let () = set_settings_bw body (bw_to_int settings.bw) in
    let () = set_settings_dvbc_qam body (dvbc_qam_to_int settings.dvbc_qam) in
    let () = set_settings_freq body settings.freq in
    let () = set_settings_plp_id body settings.plp_id in
    to_msg ~msg_code:(0x20 lor id) ~body

  let cmd_measure id settings =
    to_empty_msg ~msg_code:(0x30 lor id)

  let cmd_params id =
    to_empty_msg ~msg_code:(0x40 lor id)

  let cmd_plp_list id settings =
    to_empty_msg ~msg_code:(0x50 lor id)

  let cmd_plp_set id plp =
    let body = Cstruct.create sizeof_cmd_plp_set in
    let () = set_cmd_plp_set_plp_id body plp in
    to_msg ~msg_code:(0x60 lor id) ~body

  let check_tag_start msg =
    let prefix,_ = Cstruct.split msg sizeof_prefix in
    let tag      = get_prefix_tag_start prefix in
    if tag != tag_start then Error (Bad_tag_start tag) else Ok msg

  let check_msg_len ~length msg =
    let prefix,_  = Cstruct.split msg sizeof_prefix in
    let recvd_len = get_prefix_length prefix in
    if length != recvd_len then Error (Bad_length (length,recvd_len)) else Ok msg

  let check_msg_crc ~length msg =
    let prefix,msg' = Cstruct.split msg sizeof_prefix in
    let body,suffix = Cstruct.split msg' length in
    let crc         = (calc_crc (Cstruct.append prefix body)) in
    let crc'        = get_suffix_crc suffix in
    if crc != crc' then Error (Bad_crc (crc,crc')) else Ok msg

  let check_tag_stop ~length msg =
    let _,msg'   = Cstruct.split msg sizeof_prefix in
    let _,suffix = Cstruct.split msg' length in
    let tag      = get_suffix_tag_stop suffix in
    if tag != tag_stop then Error (Bad_tag_stop tag) else Ok msg

  let check_msg ~length msg =
    let open CCResult in
    check_tag_start msg
    >>= check_msg_len ~length
    >>= check_msg_crc ~length
    >>= check_tag_stop ~length

  let rsp_devinfo msg =
    let prefix,msg' = Cstruct.split msg sizeof_prefix in
    let length      = get_prefix_length prefix in
    let body,_      = Cstruct.split msg' length in
    let hw_cfg      = get_rsp_devinfo_hw_config body in
    { serial   = get_rsp_devinfo_serial body
    ; hw_ver   = get_rsp_devinfo_hw_ver body
    ; fpga_ver = get_rsp_devinfo_fpga_ver body
    ; soft_ver = get_rsp_devinfo_soft_ver body
    ; asi      = if (hw_cfg land 16) > 0 then true else false
    ; modules  = List.fold_left (fun acc x -> let x' = float_of_int x in
                                              if (hw_cfg land (int_of_float (2. ** x'))) > 0
                                              then x :: acc
                                              else acc)
                                []
                                (CCList.range 0 3)
    }

  let rsp_settings msg =
    let prefix,msg' = Cstruct.split msg sizeof_prefix in
    let length      = get_prefix_length prefix in
    let body,_      = Cstruct.split msg' length in
    let id          = get_prefix_msg_code prefix land 0xF in
    { lock       = if (get_settings_lock body) != 0 then false else true
    ; hw_present = if (get_settings_hw_present body) != 0 then false else true
    ; settings   = { mode     = int_to_mode (get_settings_mode body)
                   ; bw       = int_to_bw (get_settings_bw body)
                   ; dvbc_qam = int_to_dvbc_qam (get_settings_dvbc_qam body)
                   ; freq     = get_settings_freq body
                   ; plp_id   = get_settings_plp_id body
                   }
    }

  let handle _ _ id meth args _ _ _ =
    let open Redirect in
    let redirect_if_guest = redirect_if (User.eq id `Guest) in
    match meth, args with
    | `POST, ["settings"] -> redirect_if_guest not_found
    | `GET, ["devinfo"]   -> respond_string (cmd_devinfo false) ()
    | `GET, ["plps"]      -> not_found ()
    | `GET, ["meas"]      -> not_found ()
    | `GET, ["params"]    -> not_found ()
    | _ -> not_found ()

  let handlers id =
    [ (module struct
         let domain = get_api_path id
         let handle = handle () ()
       end : HANDLER) ]

  let create (b:topo_board) = { handlers = handlers b.id }

  let connect_db _ _ = ()

  let get_handlers (b:t) = b.handlers

end
