open Containers
open Board_msg_formats

let tag_start = 0x55AA
let tag_stop  = 0xFE

let max_uint16 = Unsigned.(UInt16.to_int UInt16.max_int)
let max_uint32 = Unsigned.(UInt32.to_int32 UInt32.max_int)

open Board_types

exception Parse_error

(* Misc *)

type parsed =
  { id   : int
  ; code : int
  ; body : Cbuffer.t
  ; res  : Cbuffer.t
  }

type err = Bad_tag_start        of int
         | Bad_length           of int
         | Bad_msg_code         of int
         | Bad_crc              of (int * int)
         | Bad_tag_stop         of int
         | Insufficient_payload of Cbuffer.t
         | Unknown_err          of string

let string_of_err = function
  | Bad_tag_start x        -> "incorrect start tag: "    ^ (string_of_int x)
  | Bad_length x           -> "incorrect length: "       ^ (string_of_int x)
  | Bad_msg_code x         -> "incorrect code/addr: "    ^ (string_of_int x)
  | Bad_crc (x,y)          -> "incorrect crc: expected " ^ (string_of_int x) ^ ", got " ^ (string_of_int y)
  | Bad_tag_stop x         -> "incorrect stop tag: "     ^ (string_of_int x)
  | Insufficient_payload _ -> "insufficient payload"
  | Unknown_err s          -> s

type _ request = Get_devinfo  : devinfo request
               | Reset        : unit request
               | Set_mode     : mode_req -> mode_rsp request
               | Set_plp      : plp_set_req -> plp_set_rsp request

type event = Measures of measures
           | Params   of params
           | Plp_list of plp_list [@@deriving show]

type _ event_request = Get_measure  : int -> event event_request
                     | Get_params   : int -> event event_request
                     | Get_plp_list : int -> event event_request

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

let check_tag_start buf =
  let tag = get_prefix_tag_start buf in
  if tag <> tag_start then Error (Bad_tag_start tag) else Ok buf

let check_length buf =
  let length = get_prefix_length buf in
  if (length < 2) || (length > 41) then Error (Bad_length length) else Ok buf

let check_msg_code buf =
  let code'     = get_prefix_msg_code buf in
  let (id,code) = (code' land 0x0F, code' lsr 4) in
  match (id,code) with
  | 0xE,0xE                                          -> Ok (id,code,buf)           (* ack*)
  | 0,1                                              -> Ok (id,code,buf)           (* devinfo *)
  | (x,y) when (x >= 0 && x < 4) && (y > 1 && y < 7) -> Ok (id,code,buf)           (* other *)
  | _                                                -> Error (Bad_msg_code code')

let check_msg_crc (id,code,buf) =
  let payload_len = (get_prefix_length buf) - 1 in
  let total_len   = payload_len + sizeof_prefix + sizeof_suffix in
  let msg,res     = Cbuffer.split buf total_len in
  let pfx,msg'    = Cbuffer.split msg sizeof_prefix in
  let body,sfx    = Cbuffer.split msg' payload_len in
  let tag         = get_suffix_tag_stop sfx in
  if tag <> tag_stop then Error (Bad_tag_stop tag)
  else let crc  = (calc_crc (Cbuffer.append pfx body)) in
       let crc' = get_suffix_crc sfx in
       if crc <> crc' then Error (Bad_crc (crc,crc'))
       else Ok { id; code; body; res }

let check_msg msg =
  try
    Result.(check_tag_start msg
            >>= check_length
            >>= check_msg_code
            >>= check_msg_crc)
  with
  | Invalid_argument _ -> Error (Insufficient_payload msg)
  | e                  -> Error (Unknown_err (Printexc.to_string e))

(* Requests/responses *)

let standard_to_int : standard -> int = function
  | T2 -> 1 | T -> 2 | C -> 3
let standard_of_int : int -> standard option = function
  | 1 -> Some T2 | 2 -> Some T | 3 -> Some C | _ -> None

let bw_to_int : bw -> int = function
  | Bw8 -> 1 | Bw7 -> 2 | Bw6 -> 3
let bw_of_int : int -> bw option = function
  | 1 -> Some Bw8 | 2 -> Some Bw7 | 3 -> Some Bw6 | _ -> None

(* Devinfo *)

let to_devinfo_req reset =
  let body = Cbuffer.create sizeof_cmd_devinfo in
  let () = set_cmd_devinfo_reset body (if reset then 0xFF else 0) in
  to_msg ~msg_code:0x10 ~body

let parse_devinfo_rsp_exn msg =
  try
    let hw_cfg = get_rsp_devinfo_hw_config msg in
    { serial    = get_rsp_devinfo_serial msg
    ; hw_ver    = get_rsp_devinfo_hw_ver msg
    ; fpga_ver  = get_rsp_devinfo_fpga_ver msg
    ; soft_ver  = get_rsp_devinfo_soft_ver msg
    ; asi       = if (hw_cfg land 16) > 0 then true else false
    ; receivers = List.fold_left (fun acc x -> let x' = float_of_int x in
                                               if (hw_cfg land (int_of_float (2. ** x'))) > 0
                                               then x :: acc
                                               else acc)
                                 []
                                 (List.range 0 3)
    }
  with _ -> raise Parse_error

(* Mode *)

let to_mode_req (req : mode_req) =
  let body = Cbuffer.create sizeof_mode in
  let () = set_mode_standard body (standard_to_int req.mode.standard) in
  let () = set_mode_bw body (bw_to_int req.mode.channel.bw) in
  let () = set_mode_freq body @@ Int32.of_int req.mode.channel.freq in
  let () = set_mode_plp body req.mode.channel.plp in
  to_msg ~msg_code:(0x20 lor req.id) ~body

let parse_mode_rsp_exn id msg =
  try
    let open Option in
    { id
    ; lock       = int_to_bool8 (get_mode_lock msg)       |> get_exn |> bool_of_bool8
    ; hw_present = int_to_bool8 (get_mode_hw_present msg) |> get_exn |> bool_of_bool8
    ; mode       = { standard = get_exn @@ standard_of_int (get_mode_standard msg)
                   ; channel  = { bw   = get_exn @@ bw_of_int (get_mode_bw msg)
                                ; freq = Int32.to_int @@ get_mode_freq msg
                                ; plp  = get_mode_plp msg
                                }
                   }
    }
  with _ -> raise Parse_error

(* Measure *)

let to_measure_req id =
  to_empty_msg ~msg_code:(0x30 lor id)

let parse_measures_rsp_exn id msg =
  let int_to_opt   x = if Int.equal   x max_uint16 then None else Some x in
  let int32_to_opt x = if Int32.equal x max_uint32 then None else Some x in
  try
    { id
    ; timestamp = Common.Time.Clock.now ()
    ; lock      = int_to_bool8 (get_rsp_measure_lock msg) |> Option.get_exn |> bool_of_bool8
    ; power     = Fun.(int_to_opt % get_rsp_measure_power) msg
                  |> Option.map (fun x -> -.((float_of_int x) /. 10.))
    ; mer       = Fun.(int_to_opt % get_rsp_measure_mer) msg
                  |> Option.map (fun x -> (float_of_int x) /. 10.)
    ; ber       = Fun.(int32_to_opt % get_rsp_measure_ber) msg
                  |> Option.map (fun x -> (Int32.to_float x) /. (2.**24.))
    ; freq      = Fun.(int32_to_opt % get_rsp_measure_freq) msg
                  |> Option.map Int32.to_int
    ; bitrate   = Fun.(int32_to_opt % get_rsp_measure_bitrate) msg
                  |> Option.map Int32.to_int
    }
  with _ -> raise Parse_error

(* Params *)

let to_params_req id =
  to_empty_msg ~msg_code:(0x40 lor id)

let parse_params_rsp_exn id msg : params =
  let bool_of_int x = if x = 0 then false else true in
  try
    let lock   = int_to_bool8 (get_rsp_params_lock msg) |> Option.get_exn |> bool_of_bool8 in
    let params =
      { fft             = get_rsp_params_fft msg
      ; gi              = get_rsp_params_gi  msg
      ; bw_ext          = get_rsp_params_bw_ext msg |> bool_of_int
      ; papr            = get_rsp_params_papr msg
      ; l1_rep          = get_rsp_params_l1_rep msg |> bool_of_int
      ; l1_mod          = get_rsp_params_l1_mod msg
      ; freq            = get_rsp_params_freq msg |> Int32.to_int
      ; l1_post_sz      = get_rsp_params_l1_post_sz msg
      ; l1_post_info_sz = get_rsp_params_l1_post_info_sz msg
      ; tr_fmt          = get_rsp_params_tr_fmt msg
      ; sys_id          = get_rsp_params_sys_id msg
      ; net_id          = get_rsp_params_net_id msg
      ; cell_id         = get_rsp_params_cell_id msg
      ; t2_frames       = get_rsp_params_t2_frames msg
      ; ofdm_syms       = get_rsp_params_ofdm_syms msg
      ; pp              = get_rsp_params_pp msg
      ; plp_num         = get_rsp_params_plp_num msg
      ; tx_id_avail     = get_rsp_params_tx_id_avail msg
      ; num_rf          = get_rsp_params_num_rf msg
      ; cur_rf_id       = get_rsp_params_cur_rf_id msg
      ; cur_plp_id      = get_rsp_params_cur_plp_id msg
      ; plp_type        = get_rsp_params_plp_type msg
      ; cr              = get_rsp_params_cr msg
      ; plp_mod         = get_rsp_params_plp_mod msg
      ; rotation        = get_rsp_params_rotation msg |> bool_of_int
      ; fec_sz          = get_rsp_params_fec_size msg
      ; fec_block_num   = get_rsp_params_fec_block_num msg
      ; in_band_flag    = get_rsp_params_in_band_flag msg |> bool_of_int
      }
    in
    { id
    ; timestamp = Common.Time.Clock.now ()
    ; params    = if lock then Some params else None
    }
  with _ -> raise Parse_error

(* PLP list *)

let to_plp_list_req id =
  to_empty_msg ~msg_code:(0x50 lor id)

let parse_plp_list_rsp_exn id msg : plp_list =
  try
    let plp_num = Cbuffer.get_uint8 msg 1 |> (fun x -> if x = 0xFF then None else Some x) in
    { id
    ; timestamp = Common.Time.Clock.now ()
    ; lock = int_to_bool8 (Cbuffer.get_uint8 msg 0) |> Option.get_exn |> bool_of_bool8
    ; plps = begin match plp_num with
             | Some _ -> let iter = Cbuffer.iter (fun _ -> Some 1)
                                                 (fun buf -> Cbuffer.get_uint8 buf 0)
                                                 (Cbuffer.shift msg 2) in
                         Cbuffer.fold (fun acc el -> el :: acc) iter []
                         |> List.sort compare
             | None   -> []
             end
    }
  with _ -> raise Parse_error

(* PLP set *)

let to_plp_set_req (req:plp_set_req) =
  let body = Cbuffer.create sizeof_cmd_plp_set in
  let () = set_cmd_plp_set_plp_id body req.plp in
  to_msg ~msg_code:(0x60 lor req.id) ~body

let parse_plp_set_rsp_exn id msg =
  try
    { id
    ; lock = int_to_bool8 (get_rsp_plp_set_lock msg) |> Option.get_exn |> bool_of_bool8
    ; plp  = get_rsp_plp_set_plp msg
    }
  with _ -> raise Parse_error

let deserialize buf =
  (* split buffer into valid messages and residue (if any) *)
  let parse_msg = fun {id;code;body;_} ->
    match (id,code) with
    | 0xE,0xE0 -> `R `Ack
    | 0,1      -> `R (`Devinfo body)
    | _,2      -> `R (`Settings (id, body))
    | _,3      -> `E (`Measure (id, body))
    | _,4      -> `E (`Params (id,body))
    | _,5      -> `E (`Plps (id, body))
    | _,6      -> `R (`Plp_setting (id, body))
    | _        -> `N in
  let rec f events responses b =
    if Cbuffer.len b > (sizeof_prefix + 1 + sizeof_suffix)
    then (match check_msg b with
          | Ok x    -> parse_msg x
                       |> (function
                           | `E e   -> f (e::events) responses x.res
                           | `R r   -> f events (r::responses) x.res
                           | `N     -> f events responses x.res)
          | Error e -> (match e with
                        | Insufficient_payload x -> List.rev events, List.rev responses, x
                        | _                      -> f events responses (Cbuffer.shift b 1)))
    else List.rev events, List.rev responses, b in
  let events, responses, res = f [] [] buf in
  events, responses, if Cbuffer.len res > 0 then Some res else None

let try_parse f x =
  try Some (f x) with Parse_error -> None

let parse_devinfo_rsp = function
  | `Devinfo buf -> try_parse parse_devinfo_rsp_exn buf
  | _ -> None

let parse_reset_rsp = function `Ack -> Some () | _ -> None

let parse_mode_rsp id = function
  | `Settings (idx, buf) when idx = id ->
     try_parse (fun b -> parse_mode_rsp_exn id b) buf
  | _ -> None

let parse_plp_set_rsp id = function
  | `Plp_settings (idx, buf) when idx = id ->
     try_parse (fun b -> parse_plp_set_rsp_exn id b) buf
  | _ -> None

let parse_measures_rsp id = function
  | `Measure (idx, buf) when idx = id ->
     try_parse (fun b -> Measures (parse_measures_rsp_exn id b)) buf
  | _ -> None

let parse_params_rsp id = function
  | `Params (idx, buf) when idx = id ->
     try_parse (fun b -> Params (parse_params_rsp_exn id b)) buf
  | _ -> None

let parse_plp_list_rsp id = function
  | `Plps (idx, buf) when idx = id ->
     try_parse (fun b -> Plp_list (parse_plp_list_rsp_exn id b)) buf
  | _ -> None

let is_response (type a) (req : a request) msg : a option =
  match req with
  | Reset           -> parse_reset_rsp msg
  | Get_devinfo     -> parse_devinfo_rsp msg
  | Set_mode req    -> parse_mode_rsp req.id msg
  | Set_plp req     -> parse_plp_set_rsp req.id msg

let is_event (type a) (req : a event_request) msg : a option =
  match req with
  | Get_measure  id -> parse_measures_rsp id msg
  | Get_params   id -> parse_params_rsp   id msg
  | Get_plp_list id -> parse_plp_list_rsp id msg
