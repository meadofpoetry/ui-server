let name = "service_descriptor"

let parse bs off =
  match%bitstring bs with
  | {| service_type     : 8
     ; length_1         : 8 : save_offset_to (off_1)
     ; service_provider : length_1 * 8 : save_offset_to (off_2), bitstring
     ; length_2         : 8 : save_offset_to (off_3)
     ; service          : length_2 * 8 : save_offset_to (off_4), bitstring
     |}
    ->
      let parsed =
        Application_types.MPEG_TS.service_type_to_string service_type
      in
      let service =
        match Text_decoder.decode @@ Util.Bitstring.to_cstruct service with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      let sp =
        match
          Text_decoder.decode @@ Util.Bitstring.to_cstruct service_provider
        with
        | Ok s -> s
        | Error _ -> "Unable to decode"
      in
      [
        Node.make ~parsed ~offset:off 8 "service_type" (Hex (Int service_type));
        Node.make ~offset:(off + off_1) 8 "service_provider_name_length"
          (Dec (Int length_1));
        Node.make ~offset:(off + off_2) (length_1 * 8) "service_provider"
          (String sp);
        Node.make ~offset:(off + off_3) 8 "service_name_length"
          (Dec (Int length_2));
        Node.make ~offset:(off + off_4) (length_2 * 8) "service"
          (String service);
      ]
