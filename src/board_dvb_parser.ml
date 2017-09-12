let tag_start = 0x55AA
let tag_stop  = 0xFE

let max_uint16 = Unsigned.(UInt16.to_int UInt16.max_int)
let max_uint32 = Unsigned.(UInt32.to_int UInt32.max_int)

[@@@ocaml.warning "-32"]

[%%cenum
 type bool8 =
   | True  [@id 0xFF]
   | False [@id 0] [@@uint8_t]]

let bool_of_bool8 = function
  | True  -> true
  | False -> false

[%%cenum
 type emode =
   | T2 [@id 1]
   | T  [@id 2]
   | C  [@id 3] [@@uint8_t]]

[%%cenum
 type ebw =
   | Bw8 [@id 1]
   | Bw7 [@id 2]
   | Bw6 [@id 3] [@@uint8_t]]

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
   ; rfu        : uint8_t  (* former dvb-c qam *)
   ; freq       : uint32_t
   ; plp        : uint8_t
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
   { plp_id : uint8_t
   } [@@little_endian]]

[%%cstruct
 type rsp_plp_set =
   { lock : uint8_t
   ; plp  : uint8_t
   } [@@little_endian]]

[@@@ocaml.warning "+32"]

open Common.Board.Dvb

type err = Bad_tag_start of int
         | Bad_length of int
         | Bad_msg_code of int
         | Bad_module_addr of int
         | Bad_crc of (int * int)
         | Bad_tag_stop of int
         | Unknown_err of string

let string_of_err = function
  | Bad_tag_start x   -> "incorrect start tag: "    ^ (string_of_int x)
  | Bad_length x      -> "incorrect length: "       ^ (string_of_int x)
  | Bad_msg_code x    -> "incorrect code: "         ^ (string_of_int x)
  | Bad_module_addr x -> "incorrect address: "      ^ (string_of_int x)
  | Bad_crc (x,y)     -> "incorrect crc: expected " ^ (string_of_int x) ^ ", got " ^ (string_of_int y)
  | Bad_tag_stop x    -> "incorrect stop tag: "     ^ (string_of_int x)
  | Unknown_err s     -> s

                       (*
                         type instant = Board_meta.instant

type event = Measure     of (int * rsp_measure)
           | Plps        of (int * rsp_plp_list)

type response  = Ack
               | Devinfo     of rsp_devinfo
               | Settings    of (int * rsp_settings)
               | Plp_setting of (int * rsp_plp_set)

type _ request = Req_devinfo     : response request
               | Reset           : response request
               | Req_settings    : (int * settings) -> response request
               | Req_measure     : int              -> event request
               | Req_plps        : int              -> event request
               | Req_plp_setting : int * int        -> response request

                        *)

type events = { measure : (int * rsp_measure) React.event
              ; plps    : (int * rsp_plp_list) React.event
              }

type event = Measure of (int * rsp_measure)
           | Plps    of (int * rsp_plp_list)

type response  = Ack
               | Devinfo     of rsp_devinfo
               | Settings    of (int * rsp_settings)
               | Plp_setting of (int * rsp_plp_set)
                              
                     
type _ request = Devinfo     : rsp_devinfo request
               | Reset       : unit request
               | Settings    : (int * settings) -> (int * rsp_settings) request
               | Plp_setting : int * int        -> (int * rsp_plp_set) request

type _ event_request = Measure     : int -> (int * rsp_measure) event_request
                     | Plps        : int -> (int * rsp_plp_list) event_request

(* Helper functions *)

let calc_crc (msg:Cbuffer.t) =
  let _,body = Cbuffer.split msg (sizeof_prefix - 1) in
  let iter = Cbuffer.iter (fun _ -> Some 1)
                          (fun buf -> Cbuffer.get_uint8 buf 0)
                          body in
  Cbuffer.fold (fun acc el -> el lxor acc) iter 0

(* Message constructors *)

let to_prefix ~length ~msg_code =
  let prefix = Cbuffer.create sizeof_prefix in
  let () = set_prefix_tag_start prefix tag_start in
  let () = set_prefix_length prefix length in
  let () = set_prefix_msg_code prefix msg_code in
  prefix

let to_suffix ~crc =
  let suffix = Cbuffer.create sizeof_suffix in
  let () = set_suffix_crc suffix crc in
  let () = set_suffix_tag_stop suffix tag_stop in
  suffix

let to_msg ~msg_code ~body =
  let prefix = to_prefix ~length:(Cbuffer.len body + 1) ~msg_code in
  let msg = Cbuffer.append prefix body in
  let suffix  = to_suffix ~crc:(calc_crc msg) in
  Cbuffer.append msg suffix

let to_empty_msg ~msg_code =
  let body = Cbuffer.create 1 in
  to_msg ~msg_code ~body

(* Message validation *)

let check_tag_start msg =
  let tag = get_prefix_tag_start msg in
  if tag != tag_start then Error (Bad_tag_start tag) else Ok msg

let check_length msg =
  let length = get_prefix_length msg in
  if (length < 2) || (length > 41) then Error (Bad_length length) else Ok msg

let check_msg_code msg =
  let code = get_prefix_msg_code msg in
  match (code lsr 4, code land 0x0F) with
  | 0x1,_ | 0x2,_ | 0x3,_ | 0x4,_ | 0x5,_ | 0x6,_ | 0xE,0xE -> Ok msg
  | _ -> Error (Bad_msg_code code)

let check_msg_addr msg =
  let code = get_prefix_msg_code msg in
  match (code lsr 4, code land 0x0F) with
  | 0xE,0xE               -> Ok msg
  | _,0 | _,1 | _,2 | _,3 -> Ok msg
  | _,bad_id              -> Error (Bad_module_addr bad_id)

let check_msg_crc msg =
  let prefix,msg' = Cbuffer.split msg sizeof_prefix in
  let length      = get_prefix_length prefix in
  let body,suffix = Cbuffer.split msg' (length - 1) in
  let crc         = (calc_crc (Cbuffer.append prefix body)) in
  let crc'        = get_suffix_crc suffix in
  if crc != crc' then Error (Bad_crc (crc,crc')) else Ok msg

let check_tag_stop msg =
  let prefix,msg' = Cbuffer.split msg sizeof_prefix in
  let length      = get_prefix_length prefix in
  let _,suffix    = Cbuffer.split msg' (length - 1) in
  let tag         = get_suffix_tag_stop suffix in
  if tag != tag_stop then Error (Bad_tag_stop tag) else Ok msg

let check_msg msg =
  try
    CCResult.(check_tag_start msg
              >>= check_length
              >>= check_msg_code
              >>= check_msg_addr
              >>= check_tag_stop
              >>= check_msg_crc)
  with e -> Error (Unknown_err (Printexc.to_string e))

(* Requests/responses *)

let of_mode : mode -> emode = function
  | T2 -> T2 | T -> T | C -> C
                           
let to_mode : emode -> mode = function
  | T2 -> T2 | T -> T | C -> C

let of_bw : bw -> ebw = function
  | Bw8 -> Bw8 | Bw7 -> Bw7 | Bw6 -> Bw6
                                   
let to_bw : ebw -> bw = function
  | Bw8 -> Bw8 | Bw7 -> Bw7 | Bw6 -> Bw6
                                   
(* Devinfo *)

let to_req_devinfo reset =
  let body = Cbuffer.create sizeof_cmd_devinfo in
  let () = set_cmd_devinfo_reset body (if reset then 0xFF else 0) in
  to_msg ~msg_code:0x10 ~body

let of_rsp_devinfo_exn msg =
  let hw_cfg    = get_rsp_devinfo_hw_config msg in
  { serial   = get_rsp_devinfo_serial msg
  ; hw_ver   = get_rsp_devinfo_hw_ver msg
  ; fpga_ver = get_rsp_devinfo_fpga_ver msg
  ; soft_ver = get_rsp_devinfo_soft_ver msg
  ; asi      = if (hw_cfg land 16) > 0 then true else false
  ; modules  = List.fold_left (fun acc x -> let x' = float_of_int x in
                                            if (hw_cfg land (int_of_float (2. ** x'))) > 0
                                            then x :: acc
                                            else acc)
                 []
                 (CCList.range 0 3)
  }

(* Settings *)

let to_req_settings id settings =
  let body = Cbuffer.create sizeof_settings in
  let () = set_settings_mode body (emode_to_int @@ of_mode settings.mode) in
  let () = set_settings_bw body (ebw_to_int @@ of_bw settings.bw) in
  let () = set_settings_freq body settings.freq in
  let () = set_settings_plp body settings.plp in
  to_msg ~msg_code:(0x20 lor id) ~body

let of_rsp_settings_exn msg =
  let open CCOpt in
  { lock       = int_to_bool8 (get_settings_lock msg)       |> get_exn |> bool_of_bool8
  ; hw_present = int_to_bool8 (get_settings_hw_present msg) |> get_exn |> bool_of_bool8
  ; settings   = { mode     = to_mode @@ get_exn @@ int_to_emode (get_settings_mode msg)
                 ; bw       = to_bw @@ get_exn @@ int_to_ebw (get_settings_bw msg)
                 ; freq     = get_settings_freq msg
                 ; plp      = get_settings_plp msg
                 }
  }

(* Measure *)

let to_req_measure id =
  to_empty_msg ~msg_code:(0x30 lor id)

let of_rsp_measure_exn msg =
  { lock    = int_to_bool8 (get_rsp_measure_lock msg) |> CCOpt.get_exn |> bool_of_bool8
  ; power   = get_rsp_measure_power msg
              |> (fun x -> if x = max_uint16 then None else Some (-.((float_of_int x) /. 10.)))
  ; mer     = get_rsp_measure_mer msg
              |> (fun x -> if x = max_uint16 then None else Some ((float_of_int x) /. 10.))
  ; ber     = get_rsp_measure_ber msg
              |> (fun x -> if x = Int32.of_int max_uint32 then None else Some ((Int32.to_float x) /. (2.**24.)))
  ; freq    = get_rsp_measure_freq msg
              |> (fun x -> if x = Int32.of_int max_uint32 then None else Some x)
  ; bitrate = get_rsp_measure_bitrate msg
              |> (fun x -> if x = Int32.of_int max_uint32 then None else Some x)
  }

(* Plp list *)

let to_req_plp_list id =
  to_empty_msg ~msg_code:(0x50 lor id)

let of_rsp_plp_list_exn msg =
  let plp_num     = Cbuffer.get_uint8 msg 1 |> (fun x -> if x = 0xFF then None else Some x) in
  { lock = int_to_bool8 (Cbuffer.get_uint8 msg 0) |> CCOpt.get_exn |> bool_of_bool8
  ; plps = begin match plp_num with
           | Some _ -> let iter = Cbuffer.iter (fun _ -> Some 1)
                                    (fun buf -> Cbuffer.get_uint8 buf 0)
                                    (Cbuffer.shift msg 2) in
                       Cbuffer.fold (fun acc el -> el :: acc) iter []
           | None   -> []
           end
  }

(* Plp set *)

let to_req_plp_set id plp =
  let body = Cbuffer.create sizeof_cmd_plp_set in
  let () = set_cmd_plp_set_plp_id body plp in
  to_msg ~msg_code:(0x60 lor id) ~body

let of_rsp_plp_set_exn msg =
  { lock = int_to_bool8 (get_rsp_plp_set_lock msg) |> CCOpt.get_exn |> bool_of_bool8
  ; plp  = get_rsp_plp_set_plp msg
  }

let deserialize buf =
  (* split buffer into valid messages and residue (if any) *)
  let parse_msg = fun msg ->
    try
      let id       = (get_prefix_msg_code msg) land 0x0F in
      let code     = (get_prefix_msg_code msg) land 0xF0 in
      let (_,msg') = Cbuffer.split msg sizeof_prefix in
      let (body,_) = Cbuffer.split msg' ((Cbuffer.len msg') - sizeof_suffix) in
      (match (id,code) with
       | 0xE,0xE0 -> `R `Ack
       | _,0x10   -> `R (`Devinfo body)
       | _,0x20   -> `R (`Settings (id, body))
       | _,0x30   -> `E (`Measure (id, body))
       | _,0x50   -> `E (`Plps (id, body))
       | _,0x60   -> `R (`Plp_setting (id, body))
       | _        -> Lwt_io.printf "\n\n !!! Unknown message !!! \n\n" |> ignore; `N)
    with _ -> Lwt_io.printf "\n\n !!! Corrupted message !!! \n\n" |> ignore; `N in
  let rec f events responses b =
    if Cbuffer.len b > (sizeof_prefix + 1 + sizeof_suffix)
    then (match check_msg b with
          | Ok x    -> let len     = ((get_prefix_length x) - 1) + sizeof_prefix + sizeof_suffix in
                       let msg,res = Cbuffer.split x len in
                       msg
                       |> parse_msg
                       |> (function
                           | `E x   -> f (x::events) responses res
                           | `R x   -> f events (x::responses) res
                           | `N     -> f events responses res)
          | Error e -> let _,res = Cbuffer.split b 1 in
                       Lwt_io.printf "\n\n !!! Error %s !!! \n\n" (string_of_err e) |> ignore;
                       f events responses res)
    else List.rev events, List.rev responses, b in
  let events, responses, res = f [] [] buf in
  events, responses, if Cbuffer.len res > 0 then Some res else None

let find_parse_devinfo w =
  let find   = function `Devinfo buf -> Some buf | _ -> None in
  let parser buf = Lwt.wakeup w (of_rsp_devinfo_exn buf) (* fix exn *) in
  function Some l -> CCOpt.(CCList.find_map find l >|= parser)
         | None   -> Lwt.wakeup_exn w (Failure ""); None

let find_parse_reset w =
  let find   = function `Ack -> Some () | _ -> None in
  function Some l -> CCOpt.(CCList.find_map find l >|= Lwt.wakeup w)
         | None   -> Lwt.wakeup_exn w (Failure ""); None

let find_parse_settings w id =
  let find   = function `Settings (idx, buf) when idx = id -> Some buf | _ -> None in
  let parser buf = Lwt.wakeup w (id, (of_rsp_settings_exn buf)) (* fix exn *) in
  function Some l -> CCOpt.(CCList.find_map find l >|= parser)
         | None   -> Lwt.wakeup_exn w (Failure ""); None

let find_parse_plp_settings w id =
  let parser buf = Lwt.wakeup w (id, (of_rsp_plp_set_exn buf)) (* fix exn *) in
  let find   = function `Plp_settings (idx, buf) when idx = id -> Some buf | _ -> None in
  function Some l -> CCOpt.(CCList.find_map find l >|= parser)
         | None   -> Lwt.wakeup_exn w (Failure ""); None

(* remove later *)
                     
let is_response (type a) (msg : a request) rsp =
  match msg, rsp with
  | Reset, `Ack -> true
  | Devinfo, `Devinfo _ -> true
  | Settings (id,_), `Settings (idx,_) when id = idx -> true
  | Plp_setting (id,_), `Plp_setting (idx,_) when id = idx -> true
  | _ -> false
