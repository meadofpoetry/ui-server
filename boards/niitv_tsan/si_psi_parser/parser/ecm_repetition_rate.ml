(* ETSI EN 301 192 *)

let name = "ECM_repetition_rate_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| ca_system_id        : 16
     ; ecm_repetition_rate : 16 : save_offset_to (off_1)
     ; rest                : -1 : save_offset_to (off_2), bitstring
     |}
    ->
      let private_data =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct rest with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      [
        Node.make ~offset:off 16 "CA_system_ID" (Dec (Int ca_system_id));
        Node.make ~offset:(off + off_1) 16 "ECM repetition rate"
          (Dec (Int ecm_repetition_rate));
        Node.make ~offset:(off + off_2)
          (Bitstring.bitstring_length rest)
          "private_data" (String private_data);
      ]
