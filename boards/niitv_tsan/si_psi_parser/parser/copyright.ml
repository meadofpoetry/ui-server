let name = "copyright_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| copyright_id : 32
     ; rest         : -1 : save_offset_to (off_1), bitstring
     |}
    ->
      let info =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct rest with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      [ Node.make ~offset:off 32 "copyright_identifier" (Hex (Int32 copyright_id))
      ; Node.make
          ~offset:(off + off_1)
          (Bitstring.bitstring_length rest)
          "additional copyright info"
          (String info) ]
