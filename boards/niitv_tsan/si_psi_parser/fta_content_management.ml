let name = "FTA_content_management_descriptor"

let parse_access access =
  match access with
  | 0 -> "Redistribution over the Internet is enabled."
  | 1 -> "Redistribution over the Internet is enabled but \
          only within a managed domain."
  | 2 -> "Redistribution over the Internet is enabled but \
          only within a managed domain and after a certain \
          short period of time (e.g. 24 hours)."
  | 3 -> "Redistribution over the Internet is not allowed with \
          the following exception: Redistribution over the Internet \
          within a managed domain is enabled after a specified long \
          (possibly indefinite) period of time."
  | _ -> assert false

let parse bs off =
  match%bitstring bs with
  | {| user_defined    : 1
     ; rfu             : 3 : save_offset_to (off_1)
     ; do_not_scramble : 1 : save_offset_to (off_2)
     ; cont_remote_acc : 2 : save_offset_to (off_3)
     ; do_not_apply    : 1 : save_offset_to (off_4)
     |} ->
    let parsed = parse_access cont_remote_acc in
    [ Node.make ~offset:off 1 "user_defined" (Bits (Bool user_defined))
    ; Node.make ~offset:(off + off_1) 3 "reserved_future_use" (Bits (Int rfu))
    ; Node.make ~offset:(off + off_2) 1 "do_not_scramble" (Bits (Bool do_not_scramble))
    ; Node.make ~parsed ~offset:(off + off_3) 2 "control_remote_access_over_internet"
        (Bits (Int cont_remote_acc))
    ; Node.make ~offset:(off + off_4) 1 "do_not_apply_revocation" (Bits (Bool do_not_apply))
    ]
