open Board_dektec_dtm3200_types
open Request
open Message

let ( % ) f g x = f (g x)

let cat_set_to_data_length = function
  (* Device *)
  | 0x01, x ->
     begin match x with
     | 0x01 | 0x02 -> sizeof_setting8
     | _ -> sizeof_setting32
     end
  (* Configuration *)
  | 0x02, _ -> sizeof_setting8
  (* Network *)
  | 0x03, x ->
     begin match x with
     | 0x04 -> sizeof_setting8
     | 0x05 -> 0
     | 0x06 -> sizeof_setting48
     | _ -> sizeof_setting32
     end
  (* IP receive *)
  | 0x81, x ->
     begin match x with
     | 0x03 | 0x07 | 0x0C |0x10 | 0x13 | 0x15 | 0x17 | 0x018 | 0x19 ->
        sizeof_setting32
     | 0x05 | 0x06 | 0x0A | 0x0B -> sizeof_setting16
     | 0x08 | 0x09 -> sizeof_setting64
     | _ -> sizeof_setting8
     end
  (* ASI output *)
  | 0x84, x ->
     begin match x with
     | 0x01 | 0x02 -> sizeof_setting8
     | _ -> sizeof_setting32
     end
  | _ -> assert false
type err =
  | Bad_stx of int
  | Bad_etx of int
  | Bad_length of int
  | Bad_address of int
  | Bad_category of int
  | Bad_setting of int * int
  | Bad_rw of int
  | Bad_crc of int * int
  | Insufficient_payload of Cstruct.t
  | Unknown of string

type parsed =
  { category : int
  ; setting : int
  ; rw : access
  ; body : Cstruct.t
  ; rest : Cstruct.t
  }

type err =
  | Bad_stx of int
  | Bad_etx of int
  | Bad_length of int
  | Bad_address of int
  | Bad_category of int
  | Bad_setting of int * int
  | Bad_rw of int
  | Bad_crc of int * int
  | Bad_value of Cstruct.t
  | Insufficient_payload of Cstruct.t
  | Unknown of string

module type Getter = sig
  type t
  val get : Cstruct.t -> t option
  val get_exn : Cstruct.t -> t
end

module Parse : sig
  module Int : Getter with type t = int
  module Int32 : Getter with type t = int32
  module Int64 : Getter with type t = int64
  module Ipaddr : Getter with type t = Ipaddr.V4.t
  module Bool : Getter with type t = bool
end = struct

  module type P = sig
    type t
    val of_string : string -> t
  end

  module Make_int(M : P) : (Getter with type t = M.t) = struct
    type t = M.t

    let get_exn (b : Cstruct.t) : t =
      let s = "0x" ^ (Cstruct.to_string b) in
      M.of_string s

    let get (b : Cstruct.t) : t option =
      try Some (get_exn b)
      with Failure _ -> None
  end

  module Int =
    Make_int(struct
        type t = int
        let of_string = int_of_string
      end)

  module Int32 = Make_int(Int32)

  module Int64 = Make_int(Int64)

  module Ipaddr : (Getter with type t = Ipaddr.V4.t) = struct
    type t = Ipaddr.V4.t

    let get b = match Int32.get b with
      | None -> None
      | Some x -> Some (Ipaddr.V4.of_int32 x)

    let get_exn b = Ipaddr.V4.of_int32 (Int32.get_exn b)
  end

  module Bool : (Getter with type t = bool) = struct
    type t = bool

    let get b = match Int.get b with
      | None -> None
      | Some 0 -> Some false
      | Some 1 -> Some true
      | Some _ -> None

    let get_exn b =
      match Int.get_exn b with
      | 0 -> false
      | 1 -> true
      | x -> failwith @@ Printf.sprintf "bool parser: bad value (%d)" x
  end

end

let err_to_string error =
  Printf.(
    match error with
    | Bad_stx x -> sprintf "incorrect STX: %d" x
    | Bad_etx x -> sprintf "incorrect ETX: %d" x
    | Bad_length x -> sprintf "incorrect length: %d" x
    | Bad_address x -> sprintf "incorrect address: %d" x
    | Bad_category x -> sprintf "incorrect category: %d" x
    | Bad_setting (x, y) -> sprintf "incorrect setting: %d, in category %d" y x
    | Bad_rw x -> sprintf "incorrect rw: %d" x
    | Bad_crc (x, y) -> sprintf "incorrect crc, expected %d, got %d" x y
    | Bad_value x -> sprintf "incorrect value: %s" (Cstruct.to_string x)
    | Insufficient_payload _ -> "insufficient payload"
    | Unknown s -> s)

let check_stx buf =
  try match get_prefix_stx buf with
      | x when x = stx -> Ok buf
      | x -> Error (Bad_stx x)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_address ~address buf =
  try
    let addr = get_prefix_address buf in
    match Parse.Int.get addr with
    | Some x when x = address -> Ok buf
    | Some x -> Error (Bad_address x)
    | None -> Error (Bad_value addr)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_category buf =
  try
    let cat = get_prefix_category buf in
    match Parse.Int.get cat with
    | Some x when List.mem x valid_categories -> Ok (x, buf)
    | Some x -> Error (Bad_category x)
    | None -> Error (Bad_value cat)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_setting (cat, buf) =
  try
    let set = get_prefix_setting buf in
    match Parse.Int.get set with
    | None -> Error (Bad_value set)
    | Some x ->
       if is_message_valid (cat, x)
       then Ok (cat, x, buf)
       else Error (Bad_setting (cat, x))
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_rw (cat, set, buf) =
  try
    let rw = get_prefix_rw buf in
    match access_of_int rw with
    | Some x -> Ok (cat, set, x, buf)
    | None -> Error (Bad_rw rw)
  with Invalid_argument _ -> Error (Insufficient_payload buf)

let check_rest (cat, set, rw, buf) =
  let length = match rw with
    | E -> 0
    | _ -> cat_set_to_data_length (cat, set) in
  let pfx_len =
    if List.mem cat [Device.category; Configuration.category; Network.category]
    then sizeof_prefix
    else sizeof_prefix + sizeof_setting16 in
  if Cstruct.len buf < (pfx_len + length + sizeof_suffix)
  then Error (Insufficient_payload buf)
  else (
    let pfx, msg' = Cstruct.split buf pfx_len in
    let bdy, rst' = Cstruct.split msg' length in
    let sfx, rest = Cstruct.split rst' sizeof_suffix in
    (* No need to catch exception here because we've already checked length. *)
    let crc' = Parse.Int.get_exn @@ get_suffix_crc sfx in
    let etx' = get_suffix_etx sfx in
    let crc = Serializer.calc_crc ~pfx ~bdy in
    if crc' <> crc then Error (Bad_crc (crc, crc'))
    else if etx' <> etx then Error (Bad_etx etx')
    else Ok { category = cat; setting = set; rw; body = bdy; rest })

let get_msg ~address buf =
  let ( >>= ) r f = match r with Error e -> Error e | Ok x -> f x in
  check_stx buf
  >>= check_address ~address
  >>= check_category
  >>= check_setting
  >>= check_rw
  >>= check_rest

let deserialize ~address src buf =
  let parse x = match x.rw with
    | R | W -> `Ok x
    | E ->
       Logs.warn ~src (fun m ->
           m "got error in respose: cat = %d, set = %d" x.category x.setting);
       `Error x in
  let rec f responses b =
    if Cstruct.len b > (sizeof_prefix + sizeof_suffix)
    then
      match get_msg ~address b with
      | Ok x -> f ((parse x) :: responses) x.rest
      | Error e ->
         begin match e with
         | Insufficient_payload x -> List.rev responses, x
         | e -> Logs.warn (fun m -> m "parser error: %s" @@ err_to_string e);
                f responses (Cstruct.shift b 1)
         end
    else List.rev responses, b in
  let r, res = f [] buf in
  List.rev r, if Cstruct.len res > 0 then Some res else None

let is_response (type a) (req : a Request.t) m : a option =
  let c, s = request_to_cat_set req in
  match m with
  | `Ok { category; setting; body = b; _ }
       when c = category && s = setting ->
     begin match req with
     | Devinfo x ->
        begin match x with
        | Get_fpga_ver -> Int.get b
        | Get_hw_ver -> Int.get b
        | Get_fw_ver -> Int.get b
        | Get_serial -> Int.get b
        | Get_type -> Int.get b
        end
     | Overall x ->
        let open Option.Infix in
        begin match x with
        | Get_mode -> Int.get b >>= mode_of_int
        | Get_application -> Int.get b >>= application_of_int
        | Get_storage -> Int.get b >>= storage_of_int
        | Set_mode _ -> Int.get b >>= mode_of_int
        | Set_application _ -> Int.get b >>= application_of_int
        | Set_storage _ -> Int.get b >>= storage_of_int
        end
     | Nw x ->
        begin match x with
        | Get_ip -> Ipaddr.get b
        | Get_mask -> Ipaddr.get b
        | Get_gateway -> Ipaddr.get b
        | Get_dhcp -> Bool.get b
        | Get_mac -> let rec f = fun acc s ->
                       match String.take_drop 2 s with
                       | (x,"")  -> (acc ^ x)
                       | (x,res) -> f (acc ^ x ^ ":") res in
                     (f "" (Cstruct.to_string b))
                     |> Macaddr.of_string
                     |> Result.to_opt
        | Set_ip _ -> Ipaddr.get b
        | Set_mask _ -> Ipaddr.get b
        | Set_gateway _ -> Ipaddr.get b
        | Set_dhcp _ -> Bool.get b
        | Reboot -> Some ()
        end
     | Ip x ->
        let open Option.Infix in
        begin match x with
        | Get_enable -> Bool.get b
        | Get_fec_enable -> Bool.get b
        | Get_pcr_present -> Bool.get b >|= (fun x -> Pcr_present x)
        | Get_method -> Int.get b >>= meth_of_int
        | Get_fec_delay -> Int.get b >|= (fun x -> Fec_delay x)
        | Get_fec_cols -> Int.get b >|= (fun x -> Fec_cols x)
        | Get_fec_rows -> Int.get b >|= (fun x -> Fec_rows x)
        | Get_jitter_tol -> Int.get b >|= (fun x -> Jitter_tol x)
        | Get_udp_port -> Int.get b
        | Get_delay -> Int.get b
        | Get_tp_per_ip -> Int.get b   >|= (fun x -> Tp_per_ip x)
        | Get_status          -> Int.get b   >>= status_of_int    >|= (fun x -> Status x)
        | Get_protocol        -> Int.get b   >>= protocol_of_int  >|= (fun x -> Protocol x)
        | Get_output          -> Int.get b   >>= output_of_int
        | Get_packet_size     -> Int.get b   >>= packet_sz_of_int >|= (fun x -> Packet_size x)
        | Get_bitrate         -> Int.get b   >|= (fun x -> Bitrate x)
        | Get_rate_est_mode   -> Int.get b   >>= rate_mode_of_int
        | Get_rate_change_cnt -> Int32.get b >|= (fun x -> Rate_change_cnt x)
        | Get_jitter_err_cnt  -> Int32.get b >|= (fun x -> Jitter_err_cnt x)
        | Get_lock_err_cnt    -> Int32.get b >|= (fun x -> Lock_err_cnt x)
        | Get_delay_factor    -> Int32.get b >|= (fun x -> Delay_factor x)
        | Get_lost_after_fec  -> Int64.get b >|= (fun x -> Lost_after_fec x)
        | Get_lost_before_fec -> Int64.get b >|= (fun x -> Lost_before_fec x)
        | Get_mcast_addr      -> Ipaddr.get b
        (* Setters *)
        | Set_enable _        -> Bool.get b
        | Set_fec_enable _    -> Bool.get b
        | Set_method _        -> Int.get b   >>= meth_of_int
        | Set_udp_port _      -> Int.get b
        | Set_delay _         -> Int.get b
        | Set_rate_est_mode _ -> Int.get b   >>= rate_mode_of_int
        | Set_mcast_addr _    -> Ipaddr.get b
        end
     | Asi x ->
        let open Option.Infix in
        begin match x with
        | Get_packet_size -> Int.get b   >>= asi_packet_sz_of_int
        | Get_bitrate -> Int.get b   >|= (fun x -> Asi_bitrate x)
        | Set_packet_size _ -> Int.get b   >>= asi_packet_sz_of_int
        end
     end
  | `Ok _    -> None
  | `Error _ -> None
  | _        -> None
