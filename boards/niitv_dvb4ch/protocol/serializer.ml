open Message

let to_prefix (msg : Request.msg) : Cstruct.t =
  let buf = Cstruct.create sizeof_prefix in
  let len = Cstruct.len msg.data + 1 in
  let tag = Request.tag_to_enum msg.tag in
  set_prefix_tag_start buf tag_start;
  set_prefix_length buf len;
  set_prefix_msg_code buf tag;
  buf

let to_suffix ~(crc : int) : Cstruct.t =
  let suffix = Cstruct.create sizeof_suffix in
  set_suffix_crc suffix crc;
  set_suffix_tag_stop suffix tag_stop;
  suffix

let calc_crc ~(pfx : Cstruct.t) (bdy : Cstruct.t) =
  let tag = get_prefix_msg_code pfx in
  let iter = Cstruct.iter (fun _ -> Some 1) (fun x -> Cstruct.get_uint8 x 0) bdy in
  tag lxor Cstruct.fold ( lxor ) iter 0

(** Arbitrary message constructor. *)
let serialize (type a) (request : a Request.t) : Cstruct.t =
  let msg = Request.to_msg request in
  let pfx = to_prefix msg in
  let sfx = to_suffix ~crc:(calc_crc ~pfx msg.data) in
  Cstruct.concat [pfx; msg.data; sfx]
