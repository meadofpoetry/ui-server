let to_prefix ~msg_code () =
  let pfx = Cstruct.create Message.sizeof_prefix in
  Message.set_prefix_tag_start pfx Message.tag_start;
  Message.set_prefix_msg_code pfx msg_code;
  pfx

let make_req ~msg_code ~body () =
  let pfx = to_prefix ~msg_code () in
  Cstruct.append pfx body
