open Board_dvb_types
open Message

(** Message prefix constructor.
    Prefix consists of start tag (0x55AA), message length and message code. *)
let to_prefix ~(length : int) ~(msg_code : int) : Cstruct.t =
  let prefix = Cstruct.create sizeof_prefix in
  set_prefix_tag_start prefix tag_start;
  set_prefix_length prefix length;
  set_prefix_msg_code prefix msg_code;
  prefix

(** Message suffix constructor.
    Suffix consists of checksum and stop tag (0xFE). *)
let to_suffix ~(crc : int) : Cstruct.t =
  let suffix = Cstruct.create sizeof_suffix in
  set_suffix_crc suffix crc;
  set_suffix_tag_stop suffix tag_stop;
  suffix

(** Arbitrary message constructor. *)
let make_msg ~(msg_code : int) ~(body : Cstruct.t) : Cstruct.t =
  let length = Cstruct.len body + 1 in
  let prefix = to_prefix ~length ~msg_code in
  let msg = Cstruct.append prefix body in
  let suffix = to_suffix ~crc:(calc_crc msg) in
  Cstruct.append msg suffix

(** Empty request constructor. *)
let make_empty_req ~(msg_code : int) : Cstruct.t =
  let body = Cstruct.create 1 in
  make_msg ~msg_code ~body

(** Source ID set request constructor. *)
let make_src_id_set_req (source_id : int) : Cstruct.t =
  let body = Cstruct.create sizeof_cmd_src_id in
  set_cmd_src_id_source_id body source_id;
  make_msg ~msg_code:0xD0 ~body

(** Device info get request constructor.
    If [reset] parameter is [true], the board will be restarted. *)
let make_devinfo_get_req (reset : bool) : Cstruct.t =
  let body = Cstruct.create sizeof_cmd_devinfo in
  set_cmd_devinfo_reset body (if reset then 0xFF else 0);
  make_msg ~msg_code:0x10 ~body

(** Device mode set request constructor. *)
let make_mode_set_req (id : int) (mode : Device.mode) : Cstruct.t =
  let body = Cstruct.create sizeof_mode in
  set_mode_standard body (Device.standard_to_enum mode.standard);
  set_mode_bw body (Device.bw_to_enum mode.channel.bw);
  set_mode_freq body @@ Int32.of_int mode.channel.freq;
  set_mode_plp body mode.channel.plp;
  make_msg ~msg_code:(0x20 lor id) ~body

(** Measure get request constructor. *)
let make_measure_get_req (id : int) : Cstruct.t =
  make_empty_req ~msg_code:(0x30 lor id)

(** Signal parameters get request constructor. *)
let make_params_get_req (id : int) : Cstruct.t =
  make_empty_req ~msg_code:(0x40 lor id)

(** PLP list get request constructor. *)
let make_plp_list_get_req (id : int) : Cstruct.t =
  make_empty_req ~msg_code:(0x50 lor id)
