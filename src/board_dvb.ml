open Common.Hardware
open Api_handler
open Interaction
open Board_types

module V1 : BOARD = struct

  type err = Bad_tag_start of int
           | Bad_length of (int * int)
           | Bad_crc of (int * int)
           | Bad_tag_stop of int

  type t = { handlers : (module HANDLER) list }

  let tag_start = 0x55AA
  let tag_stop  = 0xFE

  let max_uint16 = Unsigned.(UInt16.to_int UInt16.max_int)
  let max_uint32 = Unsigned.(UInt32.to_int UInt32.max_int)

  [@@@ocaml.warning "-32"]

  [%%cenum
   type bool8 =
     | True  [@id 0xFF]
     | False [@id 0] [@@uint8_t]]

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

  [%%cstruct
   type rsp_plp_set =
     { lock : uint8_t
     ; plp  : uint8_t } [@@little_endian]]

  (* [@@@ocaml.warning "+32"] *)

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
    ; freq     : int32
    ; plp_id   : int
    }

  type settings_resp =
    { settings   : settings
    ; hw_present : bool
    ; lock       : bool
    }

  type measure =
    { lock    : bool
    ; power   : float option
    ; mer     : float option
    ; ber     : float option
    ; freq    : int32 option
    ; bitrate : int32 option
    }

  type plp_list =
    { lock    : bool
    ; plps    : int list
    }

  type plp_set =
    { lock    : bool
    ; plp     : int
    }

  let calc_crc (msg:Cstruct.t) =
    let _,body = Cstruct.split msg (sizeof_prefix - 1) in
    let iter = Cstruct.iter (fun _ -> Some 1)
                            (fun buf -> Cstruct.get_uint8 buf 0)
                            body in
    Cstruct.fold (fun acc el -> el lxor acc) iter 0

  let length (msg:Cstruct.t) =
    (Cstruct.len msg) + 1

  let bool_of_bool8 = function
    | True  -> true
    | False -> false

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

  let cmd_measure id =
    to_empty_msg ~msg_code:(0x30 lor id)

  (* let cmd_params id = *)
  (*   to_empty_msg ~msg_code:(0x40 lor id) *)

  let cmd_plp_list id =
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

  let check_module_id_exn = function
    | (0 | 1 | 2 | 3) as x -> x
    | bad_id -> raise (Invalid_argument ("check_module_id_exn: bad module index " ^ (string_of_int bad_id)))

  let check_msg ~length msg =
    let open CCResult in
    check_tag_start msg
    >>= check_msg_len ~length
    >>= check_msg_crc ~length
    >>= check_tag_stop ~length

  let rsp_devinfo msg =
    try
      let prefix,msg' = Cstruct.split msg sizeof_prefix in
      let length      = get_prefix_length prefix in
      let body,_      = Cstruct.split msg' length in
      let hw_cfg      = get_rsp_devinfo_hw_config body in
      Ok { serial   = get_rsp_devinfo_serial body
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
    with e -> Error (Printexc.to_string e)

  let rsp_settings msg =
    try
      let prefix,msg' = Cstruct.split msg sizeof_prefix in
      let length      = get_prefix_length prefix in
      let body,_      = Cstruct.split msg' length in
      let id          = check_module_id_exn @@ (get_prefix_msg_code prefix) land 0xF in
      let open CCOpt in
      Ok (id, { lock       = int_to_bool8 (get_settings_lock body)       |> get_exn |> bool_of_bool8
              ; hw_present = int_to_bool8 (get_settings_hw_present body) |> get_exn |> bool_of_bool8
              ; settings   = { mode     = get_exn @@ int_to_mode (get_settings_mode body)
                             ; bw       = get_exn @@ int_to_bw (get_settings_bw body)
                             ; dvbc_qam = get_exn @@ int_to_dvbc_qam (get_settings_dvbc_qam body)
                             ; freq     = get_settings_freq body
                             ; plp_id   = get_settings_plp_id body
                             }
              })
    with e -> Error (Printexc.to_string e)

  let rsp_measure msg =
    try
      let prefix,msg' = Cstruct.split msg sizeof_prefix in
      let length      = get_prefix_length prefix in
      let body,_      = Cstruct.split msg' length in
      let id          = check_module_id_exn @@ (get_prefix_msg_code prefix) land 0xF in
      Ok (id, { lock    = int_to_bool8 (get_rsp_measure_lock body) |> CCOpt.get_exn |> bool_of_bool8
              ; power   = get_rsp_measure_power body
                          |> (fun x -> if x = max_uint16 then None else Some (-.((float_of_int x) /. 10.)))
              ; mer     = get_rsp_measure_power body
                          |> (fun x -> if x = max_uint32 then None else Some ((float_of_int x) /. 10.))
              ; ber     = Int32.to_int @@ get_rsp_measure_ber body
                          |> (fun x -> if x = max_uint32 then None else Some ((float_of_int x) /. (2.**24.)))
              ; freq    = get_rsp_measure_freq body
                          |> (fun x -> if x = Int32.of_int max_uint32 then None else Some x)
              ; bitrate = get_rsp_measure_bitrate body
                          |> (fun x -> if x = Int32.of_int max_uint32 then None else Some x)
         })
    with e -> Error (Printexc.to_string e)

  let rsp_plp_list msg =
    try
      let prefix,msg' = Cstruct.split msg sizeof_prefix in
      let length      = get_prefix_length prefix in
      let body,_      = Cstruct.split msg' length in
      let id          = check_module_id_exn @@ (get_prefix_msg_code prefix) land 0xF in
      let plp_num     = Cstruct.get_uint8 body 1 |> (fun x -> if x = 0xFF then None else Some x) in
      Ok (id, { lock = int_to_bool8 (Cstruct.get_uint8 body 0) |> CCOpt.get_exn |> bool_of_bool8
              ; plps = begin match plp_num with
                       | Some _ -> let iter = Cstruct.iter (fun _ -> Some 1)
                                                           (fun buf -> Cstruct.get_uint8 buf 0)
                                                           (Cstruct.shift body 2) in
                                   Cstruct.fold (fun acc el -> el :: acc) iter []
                       | None   -> []
                       end
         })
    with e -> Error (Printexc.to_string e)

  let rsp_plp_set msg =
    try
      let prefix,msg' = Cstruct.split msg sizeof_prefix in
      let length      = get_prefix_length prefix in
      let body,_      = Cstruct.split msg' length in
      let id          = check_module_id_exn @@ (get_prefix_msg_code prefix) land 0xF in
      Ok (id, { lock = int_to_bool8 (get_rsp_plp_set_lock body) |> CCOpt.get_exn |> bool_of_bool8
              ; plp  = get_rsp_plp_set_plp body
         })
    with e -> Error (Printexc.to_string e)

  let handle _ _ id meth args _ _ _ =
    let open Redirect in
    let redirect_if_guest = redirect_if (User.eq id `Guest) in
    match meth, args with
    | `POST, ["settings"] -> redirect_if_guest not_found
    | `POST, ["plp"]      -> redirect_if_guest not_found
    | `GET,  ["devinfo"]  -> respond_string (cmd_devinfo false) ()
    | `GET,  ["params"]   -> not_found ()
    | `GET,  ["meas"]     -> not_found ()
    | `GET,  ["plps"]     -> not_found ()
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

let create = function
  | 1 -> (module V1 : BOARD)
  | v -> failwith ("dvb board: unknown version " ^ (string_of_int v))
